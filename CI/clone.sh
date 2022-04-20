#!/bin/bash

# clones specific branch of a given repo
# if branch does not exist checks the fallback branch
# if fallback branch does not exist clones default branch

org_repo_name=$1
branch_name=$2
save_dir=$3
save_name=$4
fallback_branch=$5
default_branch=$6

repo_name="$(cut -d'/' -f2 <<<$org_repo_name)"
org_name="$(cut -d'/' -f1 <<<$org_repo_name)"

if [ "${branch_name}" = "develop" ] || [ "${branch_name}" = "master" ]; then

  echo "merging into develop or master"
  branch_name_clone=${branch_name}  #always clone develop/master of all repos when merging into develop/master
  org_repo_name_clone=${org_name}
  echo "==============================================================================="
  echo "  Merge into develop/master"
  echo "  Clone ${branch_name_clone} branch of ${org_repo_name} "
  echo "==============================================================================="
  git clone --depth 1 -b $branch_name_clone https://github.com/$org_repo_name_clone/$repo_name $save_dir/$save_name

else

  # check org_name/repo_name for branch
  git ls-remote --heads --exit-code https://github.com/$org_name/$repo_name $branch_name
  exit_code=$?

  # if branch exists in org_name/repo_name clone it
  # if not check fallback branch and clone if available
  # if fallback doesn't exist then clone default branch

  if test "${exit_code}" == "0"; then
    echo "${branch_name} branch found in ${org_repo_name}"
    branch_name_clone=${branch_name}
  else
    git ls-remote --heads --exit-code https://github.com/$org_name/$repo_name $fallback_branch
    exit_code_fallback=$?
    if test "${exit_code_fallback}" == "0"; then
      echo "${branch_name} does not exist in ${org_repo_name}"
      echo "${fallback_branch} branch found in ${org_repo_name}"
      branch_name_clone=${fallback_branch}
    else
      echo "${branch_name} or ${fallback_branch} branches do not exist in ${org_repo_name}"
      echo "clone default ${default_branch} branch"
      branch_name_clone=${default_branch}
    fi
  fi
  echo "======================================================"
  echo "Clone ${branch_name_clone} branch of ${org_repo_name} "
  echo "======================================================"
  git clone --depth 1 -b $branch_name_clone https://github.com/$org_name/$repo_name $save_dir/$save_name
fi
