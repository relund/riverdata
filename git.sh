# Git commands for push
git config --local user.name "relund"
git config --local user.email "junk@relund.dk"
git add .
git commit --allow-empty -m "Travis build: $TRAVIS_BUILD_NUMBER [ci skip]"
git push https://relund:$github_token@github.com/relund/skjern.git HEAD:master 