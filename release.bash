if [ -z $1 ]; then
    echo "A release tag such as 0.0.1 is required as the first argument."
    exit
fi

if [ -z $2 ]; then
    release_commit=$(git rev-parse master)
else
    release_commit=$2
fi

lastrelease=$(git branch -l --all 'release/*' | tail -n 1)

if [ -z $lastrelease ]; then
    firstrelease=True
    lastrelease=$(git rev-list --max-parents=0 master)
else
    firstrelease=False
fi

git checkout $lastrelease
git checkout -b "release/$1"

if [ $firstrelease == False ]; then
    git merge --allow-unrelated -X patience -X theirs --squash $release_commit
    git commit -m "$1"
else
    git commit --amend -m "$1"
fi
