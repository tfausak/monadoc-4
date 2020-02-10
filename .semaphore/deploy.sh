#! /usr/bin/env sh
checkout
zip_file="monadoc-$SEMAPHORE_GIT_SHA.zip"
cd "$( mktemp --directory )" || exit
artifact pull workflow "$zip_file"
unzip "$zip_file"
rm "$zip_file"
eval "$( aws ecr get-login --no-include-email )"
url="$( aws ecr describe-repositories --output 'text' --query 'repositories[0].repositoryUri' --repository-names monadoc )"
tag="$url:$SEMAPHORE_GIT_SHA"
docker build --tag "$tag" .
docker push "$tag"
cd - || exit
tools/terraform.sh init
plan_file="monadoc-$SEMAPHORE_GIT_SHA.tfplan"
tools/terraform.sh plan -out="$plan_file" -var commit="$SEMAPHORE_GIT_SHA"
tools/terraform.sh apply -auto-approve "$plan_file"
