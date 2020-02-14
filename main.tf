# https://www.terraform.io/docs/configuration/terraform.html
terraform {
  required_version = "~> 0.12.20"

  # https://www.terraform.io/docs/backends/types/s3.html
  backend "s3" {
    bucket = "monadoc"
    key    = "monadoc.tfstate"
    region = "us-east-1"
  }
}


# https://www.terraform.io/docs/providers/aws/index.html
provider "aws" {
  version = "~> 2.48.0"
}


locals {
  domain_apex   = "${local.name}.com"
  domain_origin = "origin.${local.domain_apex}"
  domain_www    = "www.${local.domain_apex}"
  name          = "monadoc"
  port          = 8080
}


variable "commit" {
  type = string
}


# https://www.terraform.io/docs/providers/aws/r/ecr_repository.html
resource "aws_ecr_repository" "this" {
  image_tag_mutability = "IMMUTABLE"
  name                 = local.name
}


# https://www.terraform.io/docs/providers/aws/r/ecr_lifecycle_policy.html
resource "aws_ecr_lifecycle_policy" "this" {
  repository = aws_ecr_repository.this.name

  # https://docs.aws.amazon.com/AmazonECR/latest/userguide/LifecyclePolicies.html
  policy = <<-JSON
    {
      "rules": [
        {
          "action": {
            "type": "expire"
          },
          "rulePriority": 1,
          "selection": {
            "countNumber": 100,
            "countType": "imageCountMoreThan",
            "tagStatus": "any"
          }
        }
      ]
    }
  JSON
}


# https://www.terraform.io/docs/providers/aws/r/vpc.html
resource "aws_vpc" "this" {
  cidr_block = "10.10.0.0/16"
}


# https://www.terraform.io/docs/providers/aws/r/subnet.html
resource "aws_subnet" "private_a" {
  availability_zone = "us-east-1a"
  cidr_block        = "10.10.0.0/24"
  vpc_id            = aws_vpc.this.id
}

resource "aws_subnet" "private_b" {
  availability_zone = "us-east-1b"
  cidr_block        = "10.10.1.0/24"
  vpc_id            = aws_vpc.this.id
}

resource "aws_subnet" "public_a" {
  availability_zone = "us-east-1a"
  cidr_block        = "10.10.10.0/24"
  vpc_id            = aws_vpc.this.id
}

resource "aws_subnet" "public_b" {
  availability_zone = "us-east-1b"
  cidr_block        = "10.10.11.0/24"
  vpc_id            = aws_vpc.this.id
}


# https://www.terraform.io/docs/providers/aws/r/security_group.html
resource "aws_security_group" "this" {
  vpc_id = aws_vpc.this.id

  egress {
    cidr_blocks = ["0.0.0.0/0"]
    from_port   = 0
    protocol    = "-1"
    to_port     = 0
  }

  ingress {
    cidr_blocks = ["0.0.0.0/0"]
    from_port   = 0
    protocol    = "-1"
    to_port     = 0
  }
}


# https://www.terraform.io/docs/providers/aws/r/internet_gateway.html
resource "aws_internet_gateway" "this" {
  vpc_id = aws_vpc.this.id
}


# https://www.terraform.io/docs/providers/aws/r/eip.html
resource "aws_eip" "a" {
  depends_on = [aws_internet_gateway.this]
  vpc        = true
}

resource "aws_eip" "b" {
  depends_on = [aws_internet_gateway.this]
  vpc        = true
}


# https://www.terraform.io/docs/providers/aws/r/nat_gateway.html
resource "aws_nat_gateway" "a" {
  allocation_id = aws_eip.a.id
  depends_on    = [aws_internet_gateway.this]
  subnet_id     = aws_subnet.public_a.id
}

resource "aws_nat_gateway" "b" {
  allocation_id = aws_eip.b.id
  depends_on    = [aws_internet_gateway.this]
  subnet_id     = aws_subnet.public_b.id
}


# https://www.terraform.io/docs/providers/aws/r/route_table.html
resource "aws_route_table" "private_a" {
  vpc_id = aws_vpc.this.id

  route {
    cidr_block     = "0.0.0.0/0"
    nat_gateway_id = aws_nat_gateway.a.id
  }
}

resource "aws_route_table" "private_b" {
  vpc_id = aws_vpc.this.id

  route {
    cidr_block     = "0.0.0.0/0"
    nat_gateway_id = aws_nat_gateway.b.id
  }
}

resource "aws_route_table" "public" {
  vpc_id = aws_vpc.this.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.this.id
  }
}


# https://www.terraform.io/docs/providers/aws/r/route_table_association.html
resource "aws_route_table_association" "private_a" {
  route_table_id = aws_route_table.private_a.id
  subnet_id      = aws_subnet.private_a.id
}

resource "aws_route_table_association" "private_b" {
  route_table_id = aws_route_table.private_b.id
  subnet_id      = aws_subnet.private_b.id
}

resource "aws_route_table_association" "public_a" {
  route_table_id = aws_route_table.public.id
  subnet_id      = aws_subnet.public_a.id
}

resource "aws_route_table_association" "public_b" {
  route_table_id = aws_route_table.public.id
  subnet_id      = aws_subnet.public_b.id
}


# https://www.terraform.io/docs/providers/aws/r/lb.html
resource "aws_lb" "this" {
  security_groups = [aws_security_group.this.id]
  subnets         = [aws_subnet.public_a.id, aws_subnet.public_b.id]
}


# https://www.terraform.io/docs/providers/aws/r/lb_target_group.html
resource "aws_lb_target_group" "this" {
  port        = local.port
  protocol    = "HTTP"
  target_type = "ip"
  vpc_id      = aws_vpc.this.id

  health_check {
    path = "/health-check"
  }
}


# https://www.terraform.io/docs/providers/aws/r/route53_zone.html
resource "aws_route53_zone" "this" {
  name = local.domain_apex
}


# https://www.terraform.io/docs/providers/aws/r/route53_record.html
resource "aws_route53_record" "origin" {
  name    = local.domain_origin
  type    = "A"
  zone_id = aws_route53_zone.this.id

  alias {
    evaluate_target_health = false
    name                   = aws_lb.this.dns_name
    zone_id                = aws_lb.this.zone_id
  }
}

resource "aws_route53_record" "origin_validation" {
  name    = aws_acm_certificate.origin.domain_validation_options.0.resource_record_name
  records = [aws_acm_certificate.origin.domain_validation_options.0.resource_record_value]
  ttl     = 60
  type    = aws_acm_certificate.origin.domain_validation_options.0.resource_record_type
  zone_id = aws_route53_zone.this.id
}

resource "aws_route53_record" "www" {
  name    = local.domain_www
  type    = "A"
  zone_id = aws_route53_zone.this.id

  alias {
    evaluate_target_health = false
    name                   = aws_cloudfront_distribution.www.domain_name
    zone_id                = aws_cloudfront_distribution.www.hosted_zone_id
  }
}

resource "aws_route53_record" "www_validation" {
  name    = aws_acm_certificate.www.domain_validation_options.0.resource_record_name
  records = [aws_acm_certificate.www.domain_validation_options.0.resource_record_value]
  ttl     = 60
  type    = aws_acm_certificate.www.domain_validation_options.0.resource_record_type
  zone_id = aws_route53_zone.this.id
}

resource "aws_route53_record" "apex" {
  name    = local.domain_apex
  type    = "A"
  zone_id = aws_route53_zone.this.id

  alias {
    evaluate_target_health = false
    name                   = aws_cloudfront_distribution.apex.domain_name
    zone_id                = aws_cloudfront_distribution.apex.hosted_zone_id
  }
}

resource "aws_route53_record" "apex_validation" {
  name    = aws_acm_certificate.apex.domain_validation_options.0.resource_record_name
  records = [aws_acm_certificate.apex.domain_validation_options.0.resource_record_value]
  ttl     = 60
  type    = aws_acm_certificate.apex.domain_validation_options.0.resource_record_type
  zone_id = aws_route53_zone.this.id
}


# https://www.terraform.io/docs/providers/aws/r/acm_certificate.html
resource "aws_acm_certificate" "origin" {
  domain_name       = local.domain_origin
  validation_method = "DNS"
}

resource "aws_acm_certificate" "www" {
  domain_name       = local.domain_www
  validation_method = "DNS"
}

resource "aws_acm_certificate" "apex" {
  domain_name       = local.domain_apex
  validation_method = "DNS"
}


# https://www.terraform.io/docs/providers/aws/r/acm_certificate_validation.html
resource "aws_acm_certificate_validation" "origin" {
  certificate_arn         = aws_acm_certificate.origin.arn
  validation_record_fqdns = [aws_route53_record.origin_validation.fqdn]
}

resource "aws_acm_certificate_validation" "www" {
  certificate_arn         = aws_acm_certificate.www.arn
  validation_record_fqdns = [aws_route53_record.www_validation.fqdn]
}

resource "aws_acm_certificate_validation" "apex" {
  certificate_arn         = aws_acm_certificate.apex.arn
  validation_record_fqdns = [aws_route53_record.apex_validation.fqdn]
}


# https://www.terraform.io/docs/providers/aws/r/lb_listener.html
resource "aws_lb_listener" "this" {
  certificate_arn   = aws_acm_certificate.origin.arn
  load_balancer_arn = aws_lb.this.arn
  port              = 443
  protocol          = "HTTPS"
  ssl_policy        = "ELBSecurityPolicy-2016-08"

  default_action {
    target_group_arn = aws_lb_target_group.this.arn
    type             = "forward"
  }
}


# https://www.terraform.io/docs/providers/aws/r/s3_bucket.html
resource "aws_s3_bucket" "this" {
  acl    = "public-read"
  bucket = local.domain_apex

  website {
    redirect_all_requests_to = "https://${local.domain_www}"
  }
}


# https://www.terraform.io/docs/providers/aws/r/cloudfront_distribution.html
resource "aws_cloudfront_distribution" "www" {
  aliases             = [local.domain_www]
  enabled             = true
  wait_for_deployment = false

  default_cache_behavior {
    allowed_methods        = ["DELETE", "GET", "HEAD", "OPTIONS", "PATCH", "POST", "PUT"]
    cached_methods         = ["GET", "HEAD"]
    compress               = true
    target_origin_id       = local.domain_origin
    viewer_protocol_policy = "redirect-to-https"

    forwarded_values {
      query_string = true

      cookies {
        forward = "none"
      }
    }
  }

  origin {
    domain_name = aws_route53_record.origin.fqdn
    origin_id   = local.domain_origin

    custom_origin_config {
      http_port              = 80
      https_port             = 443
      origin_protocol_policy = "https-only"
      origin_ssl_protocols   = ["TLSv1", "TLSv1.1", "TLSv1.2"]
    }
  }

  restrictions {
    geo_restriction {
      restriction_type = "none"
    }
  }

  viewer_certificate {
    acm_certificate_arn      = aws_acm_certificate.www.arn
    minimum_protocol_version = "TLSv1.1_2016"
    ssl_support_method       = "sni-only"
  }
}

resource "aws_cloudfront_distribution" "apex" {
  aliases             = [local.domain_apex]
  enabled             = true
  wait_for_deployment = false

  default_cache_behavior {
    allowed_methods        = ["GET", "HEAD"]
    cached_methods         = ["GET", "HEAD"]
    target_origin_id       = aws_s3_bucket.this.id
    viewer_protocol_policy = "redirect-to-https"

    forwarded_values {
      query_string = false

      cookies {
        forward = "none"
      }
    }
  }

  origin {
    domain_name = aws_s3_bucket.this.website_endpoint
    origin_id   = aws_s3_bucket.this.id

    custom_origin_config {
      http_port              = 80
      https_port             = 443
      origin_protocol_policy = "http-only"
      origin_ssl_protocols   = ["TLSv1", "TLSv1.1", "TLSv1.2"]
    }
  }

  restrictions {
    geo_restriction {
      restriction_type = "none"
    }
  }

  viewer_certificate {
    acm_certificate_arn      = aws_acm_certificate.apex.arn
    minimum_protocol_version = "TLSv1.1_2016"
    ssl_support_method       = "sni-only"
  }
}


# https://www.terraform.io/docs/providers/aws/r/ecs_cluster.html
resource "aws_ecs_cluster" "this" {
  name = local.name
}


# https://www.terraform.io/docs/providers/aws/d/iam_policy_document.html
data "aws_iam_policy_document" "this" {
  statement {
    actions = ["sts:AssumeRole"]

    principals {
      identifiers = ["ecs-tasks.amazonaws.com"]
      type        = "Service"
    }
  }
}


# https://www.terraform.io/docs/providers/aws/r/iam_role.html
resource "aws_iam_role" "this" {
  assume_role_policy = data.aws_iam_policy_document.this.json
}


# https://www.terraform.io/docs/providers/aws/r/iam_role_policy_attachment.html
resource "aws_iam_role_policy_attachment" "this" {
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
  role       = aws_iam_role.this.name
}


# https://www.terraform.io/docs/providers/aws/r/cloudwatch_log_group.html
resource "aws_cloudwatch_log_group" "this" {
  retention_in_days = 30
}


# https://www.terraform.io/docs/providers/aws/r/ecs_task_definition.html
resource "aws_ecs_task_definition" "this" {
  cpu                      = 256
  execution_role_arn       = aws_iam_role.this.arn
  family                   = local.name
  memory                   = 512
  network_mode             = "awsvpc"
  requires_compatibilities = ["FARGATE"]

  # https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ContainerDefinition.html
  container_definitions = <<-JSON
    [
      {
        "essential": true,
        "image": "${aws_ecr_repository.this.repository_url}:${var.commit}",
        "logConfiguration": {
          "logDriver": "awslogs",
          "options": {
            "awslogs-group": "${aws_cloudwatch_log_group.this.name}",
            "awslogs-region": "us-east-1",
            "awslogs-stream-prefix": "${local.name}"
          }
        },
        "name": "${local.name}",
        "portMappings": [
          {
            "containerPort": ${local.port}
          }
        ]
      }
    ]
  JSON
}


# https://www.terraform.io/docs/providers/aws/r/ecs_service.html
resource "aws_ecs_service" "this" {
  cluster         = aws_ecs_cluster.this.arn
  desired_count   = 1
  launch_type     = "FARGATE"
  name            = local.name
  task_definition = aws_ecs_task_definition.this.arn

  load_balancer {
    container_name   = local.name
    container_port   = local.port
    target_group_arn = aws_lb_target_group.this.arn
  }

  network_configuration {
    security_groups = [aws_security_group.this.id]
    subnets         = [aws_subnet.private_a.id, aws_subnet.private_b.id]
  }
}
