# sh scripts/nix-build.sh
cp result/bin/try-phi.jsexe/all.js ../all.js
cp index.html ../index.html
git checkout publish
mv ../all.js all.js
mv ../index.html index.html
# git add .
# git commit -m "publish"
# git push origin publish
# git checkout main