Clojure script implementation of the yome-widget.

Requires:
  Leiningen:  http://leiningen.org
  
Developement:
  Fork bhauman/yome-widget project into own git account (on github.com)
  
  Clone forked yome-widget: 
    git clone https://github.com/username/yome-widget.git  Insert github username in path
    
  Add upstream remote:
    cd yome-widget/
    git remote add upstream https://github.com/bhauman/yome-widget.git
    
  Update forked repository:
    git fetch upstream
    git merge upstream/master

  Run Leiningin Figwheel in yome-widget directory:
    lein figwheel

  Wait for "Prompt will show when figwheel connects to your application"

  Open Browser to http://localhost:3449
    -port 3449 is default, which is set in project.clj 
               under :figwheel :server-port tag

  Open ./yome-widget/src/yome/core.cljs in your preferred editor

  Save edits, and they will display in the browser window automagically.

Update Master Node with Changes:
  Merge Git Repositories:
    git add .
    git commit -m "Descriptive Comment about changes"
    git push original master
    
  Request Pull:
    Go to forked repository in github.com
    Set Branch to master
    Click "New Pull Request Button"
    Review Changes
    Click Create Pull Request

To Deploy:
  Compile yome.js:
    Run lein figwheel

    Connect to localhost:3449 (this is just to get the REPL prompt

    At cljs.user=> prompt type:
      (clean-builds)
      (build-once min)

    exit REPL (^C)
    
  Merge Git Repositories:
    copy /yome-widget/resources/public/js/compiled/yome.js to github fork
      This should be placed under the gh-pages branch in the javascripts folder.

  Request Pull (see above)
    
