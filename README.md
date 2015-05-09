# EMACS configuration files

<p>To install, go to init.el and set fresh-install to 1 or t (should be nil)
<p>Other than that there are other things you will have to do

# General Installation
<p>You will need the following packages:
	   <p>-jedi (python backend)
	   <p>-flake8 (pyflakes/python syntax checker)
	   <p>-ipython

# Linux Installation
<p> Just install the packages and you will be good to go
```
$ sudo apt-get install jedi pyflakes ipython
```

# MAC OS X Installation
<p> MAC OS is a lot more involved:
```
$ sudo easy_install pip
```
<p> That will install pip on your machine. Use that to install the others:
```
$ sudo pip install jedi flake8 ipython
```
<p> You will now need to add the python directory to $PATH

*YOU CAN ALSO USE MACPORTS AND EVERYTHING SHOULD WORK OUT OF THE BOX*
```
$ sudo port install py-ipython py-jedi py-pyflakes 
```
<a href="https://astrofrog.github.io/macports-python/">Here</a> for installing ipython using macports.
<p>Macports will automatically add the python things to $PATH. If all else fails, just install using pip.

Setting up terminal:
<a href="http://superuser.com/questions/239994/how-to-have-full-directory-path-always-shown-in-mac-terminal-like-linux-termina">Set up the shell to look like linux's</a> then <a href="http://stackoverflow.com/questions/958194/how-to-make-emacs-shell-execute-init-file-automatically">make it execute automatically.
