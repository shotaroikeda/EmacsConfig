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
            print "You must run this script as sudo. Please run: "
            print "sudo python install.py"
            raise PermissionDeniedError("Run install script as sudo.")

        else:
            sp.call(["rm", "/etc/sample.txt"])

    # No Error: ran as sudo

    # Agree to xcode installation first!
    sp.call(["xcodebuild", "-license"])
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
        print "Could not set fresh-install to t."
        print "If you ran this script once already, just ignore it."
        print "Otherwise you might be in some trouble."
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
            print "Detected homebrew"
        except OSError:
            print "homebrew is not installed"

    if not skip:
        try:
            r_code = sp.call(["port", "installed"], stdout=DEVNULL, stderr=sp.STDOUT)
            package_manager = "macports"
            print "Detected macports"
        except OSError:
            print "macports is not installed"

    # Download macports if it does not exist. Brew support to come later
    if not package_manager == "macports":
        if package_manager == "homebrew":
            print "homebrew is not supported at the moment."
            print "Script will download macports instead."
        install_macports(download_macports())

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
    print "Downloading: %s Bytes: %s" % (file_name, file_size)
    
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
    sp.call(["/usr/sbin/installer", "-pkg", dir_to_macport,
             "-target", "/opt/local"])
    print "[MacPorts] Finished installing!"


def linuxinstall():
    pass


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
