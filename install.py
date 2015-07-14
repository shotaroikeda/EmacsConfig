import os
import sys
import subprocess as sp
import urllib2


class PermissionDeniedError(Exception):
    pass


def macosinstall(skip=False):
    # Have to check for sudo permissions first
    if not skip:
        r_code = sp.check_call(["touch", "/etc/sample.txt"]) # see if you can write to this directory first

        if r_code == 1:
            print " [EmacsConfig] You must run this script as sudo. Please run: "
            print " [EmacsConfig] sudo python install.py"
            raise PermissionDeniedError(" [EmacsConfig] Run install script as sudo.")

        else:
            sp.check_call(["rm", "/etc/sample.txt"])

    # No Error: ran as sudo

    # Agree to xcode installation first!
    print " [xcodebuild] Asking user to agree with license (just in case)"
    try:
        sp.check_call(["xcodebuild", "-license"])
    except sp.CalledProcessError:
        print " [xcodebuild] Please install xcode if there was an error, otherwise things are ok."
        print " [xcodebuild] Will force install MacPorts."
        print " [xcodebuild] To avoid future issues, please install XCode from the App Store."
    # hide input in /dev/null
    DEVNULL = open(os.devnull, "w")

    sp.call(["mkdir", "install_files"], stdout=DEVNULL, stderr=sp.STDOUT)
    # put all of the downloaded files in install
    home_dir = '/'.join(os.getcwd().split('/')[0:3])
    gitconfigdir = os.getcwd()

    # Set init.el (setq fresh-install t)
    f = open('init.el', 'r')
    lines = f.readlines()
    try:
        change = lines.index("(setq fresh-install nil)\n")
        lines[change] = '(setq fresh-install t)\n'
    except ValueError:
        print " [EmacsConfig] Could not set fresh-install to t."
        print " [EmacsConfig] If you ran this script once already, just ignore it."
        print " [EmacsConfig] Otherwise you might be in some trouble."

    f.close()
    f = open('init.el', 'w')
    f.writelines(lines)
    f.close()
    # Really slow code so, keep init.el to minimum
    # Check if a package manager is already installed
    package_manager = None
    # check for brew
    if not skip:
        try:
            r_code = sp.call(["brew", "list"], stdout=DEVNULL, stderr=sp.STDOUT)
            package_manager = "homebrew"
            print " [EmacsConfig] Detected homebrew"
        except OSError:
            print " [EmacsConfig] homebrew is not installed"

    if not skip:
        try:
            r_code = sp.call(["port", "installed"], stdout=DEVNULL, stderr=sp.STDOUT)
            package_manager = "macports"
            print " [EmacsConfig] Detected macports"
        except OSError:
            print " [EmacsConfig] macports is not installed"

    # Download macports if it does not exist. Brew support to come later
    if not package_manager == "macports":
        if package_manager == "homebrew":
            print " [EmacsConfig] homebrew is not supported at the moment."
            print " [EmacsConfig] Script will download macports instead."
        install_macports(download_macports())
        configure_macports(home_dir)

    print " [MacPorts] Updating MacPorts..."
    sp.call(["port", "selfupdate"])
    print " [MacPorts] Finished updating MacPorts"

    print " [MacPorts] Installing C language dependencies..."
    sp.call(["port", "install", "clang-3.7"])
    print " [MacPorts] Finished installing C language dependencies."

    print " [MacPorts] Updating all dependencies..."
    sp.call(["port", "upgrade", "outdated"])
    print " [MacPorts] Finished updating all components!"

    # Configure pip
    print " [EmacsConfig] Checking for pip."
    try:
        sp.check_call(["pip", "freeze"], stdout=DEVNULL, stderr=sp.STDOUT)
        print " [EmacsConfig] pip is installed correctly."
    except OSError:
        print " [EmacsConfig] Installing pip."
        sp.call(["easy_install", "pip"])
        print " [EmacsConfig] pip is installed."

    print " [EmacsConfig] This will use pip to install the packages."
    print " [EmacsConfig] You can also change these to the MacPorts installation."

    pip_install()

    print " [EmacsConfig] Checking for PATH environment variable."
    check_path(home_dir)

    print " [EmacsConfig] Installing EMACS"
    sp.call(["port", "install", "emacs-app"])
    print " [EmacsConfig] Finished Installing Emacs."

    print " [EmacsConfig] Cleaning up then finalizing..."
    sp.call(["rm", "-rf", gitconfigdir+"install_files"])

    print " [EmacsConfig] Making a copy of configuration to ~/.emacs.d"
    print " [EmacsConfig] Please check here to update emacs later."

    sp.call(["ln", "-s", gitconfigdir, home_dir+"/.emacs.d"])
    sp.call(["chmod", "777", "-R", gitconfigdir])

    print " [EmacsConfig] Updating iPython to recognize installed packages..."
    ipython_mac_profile(home_dir)
    print " [EmacsConfig] Done!"

    print " [EmacsConfig] Finished installing! Please do not delete your EmacsConfig folder."
    print " [EmacsConfig] Use the ~/.emacs.d folder from now on. Thank you."

    # close the devnull file
    DEVNULL.close()


def ipython_mac_profile(homedir):
    try:
        sp.check_call(["/bin/bash", "-i", "-c", "ipython", "profile", "create"])
    except sp.CalledProcessError:
        print " [EmacsConfig] There was an error setting the ipython profile."
        print " [EmacsConfig] You may need to configure iPython another time."
        print " [EmacsConfig] Rerunning the script may help."
        print " [EmacsConfig] The script may exit with an error, but Emacs will still work."
        print " [EmacsConfig] Just launch Emacs under Application/MacPorts/Emacs"
    l1 = 'c.InteractiveShellApp.exec_lines = [\n'
    l2 = '\t\'import sys,os; sys.path.append(os.getcwd()); sys.path.append(\\\'/Library/Python/2.7/site-packages\\\')\'\n'
    l3 = '\t]\n'
    ipython_config = open(homedir+"/.ipython/profile_default/ipython_config.py", "a")
    ipython_config.writelines([l1, l2, l3])
    ipython_config.close()


def check_path(homedir):
    print " [EmacsConfig] Checking for PATH"
    f = open("/etc/paths", "r")
    path = f.readlines()
    f.close()
    # Check for important $PATHs
    # pip install directory:
    if not '/usr/local/bin\n' in path:
        path.append('/usr/local/bin\n')
    if not homedir+'/Library/Python/2.7/lib/python/site-packages\n' in path:
        path.append(homedir+'/Library/Python/2.7/lib/python/site-packages\n')
    if not '/Library/Python/2.7/site-packages\n' in path:
        path.append(homedir+'/Library/Python/2.7/site-packages\n')
    if not '/System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/plat-darwin\n' in path:
        path.append('/System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/plat-darwin\n')
    if not '/System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/plat-mac\n' in path:
        path.append('/System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/plat-mac\n')
    if not '/System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/plat-mac/lib-scriptpackages\n' in path:
        path.append('/System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/plat-mac/lib-scriptpackages\n')
    if not '/System/Library/Frameworks/Python.framework/Versions/2.7/Extras/lib/python\n' in path:
        path.append('/System/Library/Frameworks/Python.framework/Versions/2.7/Extras/lib/python\n')
    if not '/Library/Python/2.7/site-packages/IPython/extensions\n' in path:
        path.append('/Library/Python/2.7/site-packages/IPython/extensions\n')
    if not homedir+'/.ipython\n' in path:
        path.append(homedir+'/.ipython\n')
    if not homedir+'/.emacs.d\n' in path:
        path.append(homedir+'/.emacs.d\n')

    print " [EmacsConfig] Backing up your current PATH..."
    sp.call(["mv", "/etc/paths", "/etc/paths-backup-emacsconf"])
    print " [EmacsConfig] Finished back up."
    print " [EmacsConfig] Writing new PATH."
    new_path = open("/etc/paths", "w+")
    new_path.writelines(path)
    print " [EmacsConfig] Finished Creating new PATH."
    print " [EmacsConfig] Setting Permissions..."
    sp.call(["chmod", "644", "/etc/paths"])
    print " [EmacsConfig] Finished!"


def download_macports():
    url = "https://distfiles.macports.org/MacPorts/MacPorts-2.3.3-10.10-Yosemite.pkg"

    file_name = url.split('/')[-1]
    u = urllib2.urlopen(url)
    f = open("install_files/"+file_name, 'wb')
    meta = u.info()
    file_size = int(meta.getheaders("Content-Length")[0])
    print " [EmacsConfig] Downloading: %s Bytes: %s" % (file_name, file_size)
    
    file_size_dl = 0
    block_sz = 8192
    while True:
        buffer = u.read(block_sz)
        if not buffer:
            break
        
        file_size_dl += len(buffer)
        f.write(buffer)
        status = r"%10d  [%3.2f%%]" % (file_size_dl, file_size_dl * 100. / file_size)
        status = status + chr(8)*(len(status)+1)
        print status,
        
    f.close()
    return "install_files/"+file_name


def install_macports(dir_to_macport):
    print "[MacPorts] Installing MacPorts..."
    sp.check_call(["/usr/sbin/installer", "-pkg", dir_to_macport,
                   "-target", "/"])
    print " [MacPorts] Finished installing!"


def configure_macports(homedir):
    # Configuring path variables to be able to use macports

    # Backup current .bash_profile
    print " [EmacsConfig] Backing up current .bash_profile..."
    sp.call(["mv", homedir+"/.bash_profile", homedir+"/.bash_profile-backupfrom-emacsconfig"])
    print " [EmacsConfig] Finished backup."

    print " [EmacsConfig] Creating new ~/.bash_profile"
    new_profile = open(homedir+"/.bash_profile", "a+")
    print " [EmacsConfig] Adding existing contents to .bash_profile"
    try:
        old_profile = open(homedir+"/.bash_profile-backupfrom-emacsconfig", "r")
        new_profile.write(old_profile.read())
        old_profile.close()
    except IOError:
        print " [EmacsConfig] There was no previous configuration found."

    print " [EmacsConfig] Finished copying configurations."
    print " [EmacsConfig] Adding new configurations."
    new_profile.write("\n# path for macports is added below")
    new_profile.write("\nexport PATH=\"/opt/local/bin:/opt/local/sbin:$PATH\"\n")
    new_profile.write("\n# Please do not modify this file unless you know what you are doing.\n")
    new_profile.close()
    # Waiting for the file to be saved and closed before proceeding
    sp.check_call(["chmod", "a+x", homedir+"/.bash_profile"])

    try:
        sp.check_call(["/bin/bash", "-i", "-c", "source", homedir+"/.bash_profile"])
    except sp.CalledProcessError:
        print " [EmacsConfig] There was an error setting the new bash_profile."
        print " [EmacsConfig] This is a normal procedure, will ignore errors for now."
        print " [EmacsConfig] If there is an error later, try rerunning the script."

    print " [EmacsConfig] Checking if MacPorts is installed and running..."
    DEVNULL = open(os.devnull, "w")
    try:
        sp.check_call(["port", "installed"], stdout=DEVNULL, stderr=sp.STDOUT)
        print " [EmacsConfig] MacPorts is working properly!"
    except sp.CalledProcessError:
        print " [EmacsConfig] MacPorts is still not configured. Please install manually."
        print " [EmacsConfig] Cleaning up install directory...."
        sp.check_call(["rm", "-rf", "/opt"], stdout=DEVNULL, stderr=sp.STDOUT)
        sp.check_call(["rm", homedir+"/.bash_profile"], stdout=DEVNULL, stderr=sp.STDOUT)
        sp.check_call(["mv", homedir+"/.bash_profile-backupfrom-emacsconfig", homedir+"/.bash_profile"],
                stdout=DEVNULL, stderr=sp.STDOUT)

    DEVNULL.close()


def linuxinstall():
    pass


def pip_install():
    print " [pip] Installing Python dependencies."
    # Install pip jedi
    sp.call(["sudo", "pip", "install", "jedi"])
    # Install pip flake8
    sp.call(["sudo", "pip", "install", "flake8"])
    # Install pip pyflakes
    sp.call(["sudo", "pip", "install", "pyflakes"])
    # Install ipython
    sp.call(["sudo", "pip", "install", "ipython"])
    print " [pip] Finished installing Python dependencies!"


def run():
    thisos = sys.platform

    if thisos == 'darwin':
        macosinstall()
    elif thisos == 'linux':
        linuxinstall()
    else:
        print "This OS is not currently supported"


def test_sudo():
    print macosinstall()

if __name__ == '__main__':
    run()
