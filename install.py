import os
import sys
import subprocess as sp
import urllib2

class PermissionDeniedError(Exception):
    pass

def macosinstall(skip=False):
    # Have to check for sudo permissions first
    if not skip:
        r_code = sp.call(["touch", "/etc/sample.txt"]) # see if you can write to this directory first

        if r_code == 1:
            print " [EmacsConfig] You must run this script as sudo. Please run: "
            print " [EmacsConfig] sudo python install.py"
            raise PermissionDeniedError(" [EmacsConfig] Run install script as sudo.")

        else:
            sp.call(["rm", "/etc/sample.txt"])

    # No Error: ran as sudo

    # Agree to xcode installation first!
    print " [xcodebuild] Asking user to agree with license (just in case)"
    sp.call(["xcodebuild", "-license"])
    print " [xcodebuild] Please install xcode if there was an error, otherwise things are ok."
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

    # close the devnull file
    DEVNULL.close()

def download_macports():
    # Make sure to remove this file before pusing the final build!

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
    # Create /opt/local
    # if not os.path.exists("/opt/local/"):
    #     if not os.path.exists("/opt/"):
    #         sp.call(["mkdir", "/opt/"])
    #     sp.call(["mkdir", "/opt/local/"])

    print "[MacPorts] Installing MacPorts..."
    sp.call(["/usr/sbin/installer", "-pkg", dir_to_macport,
             "-target", "/"])
    print " [MacPorts] Finished installing!"


def configure_macports(homedir):
    # Configuring path variables to be able to use macports
    # Might as well configure all the $PATHs...

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
    sp.call(["chmod", "a+x", homedir+"/.bash_profile"])
    sp.call(["source", homedir+"/.bash_profile"])

    print " [EmacsConfig] Checking if MacPorts is installed and running..."
    DEVNULL = open(os.devnull, "w")
    try:
        sp.call(["port", "installed"], stdout=DEVNULL, stderr=sp.STDOUT)
        print " [EmacsConfig] MacPorts is working properly!"
    except OSError:
        print " [EmacsConfig] MacPorts is still not configured. Please install manually."
        print " [EmacsConfig] Cleaning up install directory...."
        sp.call(["rm", "-rf", "/opt"], stdout=DEVNULL, stderr=sp.STDOUT)
        sp.call(["rm", homedir+"/.bash_profile"], stdout=DEVNULL, stderr=sp.STDOUT)
        sp.call(["mv", homedir+"/.bash_profile-backupfrom-emacsconfig", homedir+"/.bash_profile"],
                stdout=DEVNULL, stderr=sp.STDOUT)

    DEVNULL.close()


def linuxinstall():
    pass

def makefile(homedir):
    f = open(homedir+"/file.txt", "a+")
    f.write("hello world\n")
    f.close()

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
