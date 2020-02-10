variable "commit" {
  type = string
}


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


# https://www.terraform.io/docs/providers/aws/r/ecr_repository.html
resource "aws_ecr_repository" "this" {
  image_tag_mutability = "IMMUTABLE"
  name                 = "monadoc"
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
resource "aws_subnet" "private" {
  availability_zone = "us-east-1a"
  cidr_block        = "10.10.0.0/24"
  vpc_id            = aws_vpc.this.id
}

resource "aws_subnet" "private_extra" {
  availability_zone = "us-east-1b"
  cidr_block        = "10.10.3.0/24"
  vpc_id            = aws_vpc.this.id
}

resource "aws_subnet" "public" {
  availability_zone = "us-east-1a"
  cidr_block        = "10.10.1.0/24"
  vpc_id            = aws_vpc.this.id
}

resource "aws_subnet" "public_extra" {
  availability_zone = "us-east-1b"
  cidr_block        = "10.10.2.0/24"
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
resource "aws_eip" "this" {
  depends_on = [aws_internet_gateway.this]
  vpc        = true
}

resource "aws_eip" "this_extra" {
  depends_on = [aws_internet_gateway.this]
  vpc        = true
}


# https://www.terraform.io/docs/providers/aws/r/nat_gateway.html
resource "aws_nat_gateway" "this" {
  allocation_id = aws_eip.this.id
  depends_on    = [aws_internet_gateway.this]
  subnet_id     = aws_subnet.public.id
}

resource "aws_nat_gateway" "this_extra" {
  allocation_id = aws_eip.this_extra.id
  depends_on    = [aws_internet_gateway.this]
  subnet_id     = aws_subnet.public_extra.id
}


# https://www.terraform.io/docs/providers/aws/r/route_table.html
resource "aws_route_table" "private" {
  vpc_id = aws_vpc.this.id

  route {
    cidr_block     = "0.0.0.0/0"
    nat_gateway_id = aws_nat_gateway.this.id
  }
}

resource "aws_route_table" "private_extra" {
  vpc_id = aws_vpc.this.id

  route {
    cidr_block     = "0.0.0.0/0"
    nat_gateway_id = aws_nat_gateway.this_extra.id
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
resource "aws_route_table_association" "private" {
  route_table_id = aws_route_table.private.id
  subnet_id      = aws_subnet.private.id
}

resource "aws_route_table_association" "private_extra" {
  route_table_id = aws_route_table.private_extra.id
  subnet_id      = aws_subnet.private_extra.id
}

resource "aws_route_table_association" "public" {
  route_table_id = aws_route_table.public.id
  subnet_id      = aws_subnet.public.id
}

resource "aws_route_table_association" "public_extra" {
  route_table_id = aws_route_table.public.id
  subnet_id      = aws_subnet.public_extra.id
}


# https://www.terraform.io/docs/providers/aws/r/route53_zone.html
resource "aws_route53_zone" "this" {
  name = "monadoc.com"
}


# https://www.terraform.io/docs/providers/aws/r/lb.html
resource "aws_lb" "this" {
  security_groups = [aws_security_group.this.id]
  subnets         = [aws_subnet.public.id, aws_subnet.public_extra.id]
}


# https://www.terraform.io/docs/providers/aws/r/lb_target_group.html
resource "aws_lb_target_group" "this" {
  port        = 8080
  protocol    = "HTTP"
  target_type = "ip"
  vpc_id      = aws_vpc.this.id

  health_check {
    path = "/health-check"
  }
}


# https://www.terraform.io/docs/providers/aws/r/lb_listener.html
resource "aws_lb_listener" "this" {
  load_balancer_arn = aws_lb.this.arn
  port              = 80

  default_action {
    target_group_arn = aws_lb_target_group.this.arn
    type             = "forward"
  }
}


# https://www.terraform.io/docs/providers/aws/r/ecs_cluster.html
resource "aws_ecs_cluster" "this" {
  name = "default"
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
  family                   = "monadoc"
  memory                   = 512
  network_mode             = "awsvpc"
  requires_compatibilities = ["FARGATE"]

  # https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ContainerDefinition.html
  container_definitions = <<-JSON
    [
      {
        "environment": [
          {
            "name": "monadoc_commit",
            "value": "${var.commit}"
          }
        ],
        "essential": true,
        "image": "${aws_ecr_repository.this.repository_url}:${var.commit}",
        "logConfiguration": {
          "logDriver": "awslogs",
          "options": {
            "awslogs-group": "${aws_cloudwatch_log_group.this.name}",
            "awslogs-region": "us-east-1",
            "awslogs-stream-prefix": "monadoc"
          }
        },
        "name": "monadoc",
        "portMappings": [
          {
            "containerPort": 8080
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
  name            = "monadoc"
  task_definition = aws_ecs_task_definition.this.arn

  load_balancer {
    container_name   = "monadoc"
    container_port   = 8080
    target_group_arn = aws_lb_target_group.this.arn
  }

  network_configuration {
    security_groups = [aws_security_group.this.id]
    subnets         = [aws_subnet.private.id, aws_subnet.private_extra.id]
  }
}


# https://www.terraform.io/docs/providers/aws/r/cloudfront_distribution.html
resource "aws_cloudfront_distribution" "this" {
  enabled = true

  default_cache_behavior {
    allowed_methods        = ["DELETE", "GET", "HEAD", "OPTIONS", "PATCH", "POST", "PUT"]
    cached_methods         = ["GET", "HEAD"]
    compress               = true
    target_origin_id       = "monadoc"
    viewer_protocol_policy = "redirect-to-https"

    forwarded_values {
      query_string = true

      cookies {
        forward = "none"
      }
    }
  }

  origin {
    domain_name = aws_lb.this.dns_name
    origin_id   = "monadoc"

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
    cloudfront_default_certificate = true
  }
}
