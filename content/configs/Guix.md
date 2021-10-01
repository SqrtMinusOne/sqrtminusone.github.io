+++
title = "Guix"
author = ["Pavel"]
draft = false
+++

[GNU Guix](https://guix.gnu.org/) is (1) a transactional package manager and (2) a GNU/Linux distribution.

My personal selling points are declarative package configuration and transactional upgrades.

References:

-   [Official help](https://guix.gnu.org/en/help/)
-   [System Crafters wiki](https://wiki.systemcrafters.cc/guix)
-   [Pjotr Prins' Guix notes](https://gitlab.com/pjotrp/guix-notes)
-   [Davil Wilson's YouTube series](https://www.youtube.com/watch?v=iBaqOK75cho&list=PLEoMzSkcN8oNxnj7jm5V2ZcGc52002pQU)

<div class="ox-hugo-toc toc">
<div></div>

<div class="heading">Table of Contents</div>

- [Profiles](#profiles)
    - [Activate profiles](#activate-profiles)
    - [Update profiles](#update-profiles)
- [Channels](#channels)
- [Systems](#systems)
    - [Base configuration](#base-configuration)
    - [indigo](#indigo)
    - [eminence](#eminence)
    - [azure](#azure)
- [System installation](#system-installation)
    - [Preparation](#preparation)
    - [Installation](#installation)
    - [After installation](#after-installation)
- [Misc software & notes](#misc-software-and-notes)
    - [VPN](#vpn)
        - [vpn-start](#vpn-start)
        - [vpn-stop](#vpn-stop)
    - [flatpak](#flatpak)
    - [conda](#conda)
    - [Slack](#slack)
    - [virt-manager](#virt-manager)
    - [wakatime-cli](#wakatime-cli)
    - [Manifest](#manifest)

</div>
<!--endtoc-->


## Profiles {#profiles}

A profile is a way to group Guix packages. Amongst its advantages, profiles can be defined by manifests, which in turn can be stored in VCS.

References:

-   [Guix Profiles in Practice](https://guix.gnu.org/en/cookbook/en/html%5Fnode/Guix-Profiles-in-Practice.html)


### Activate profiles {#activate-profiles}

A script to activate guix profiles. Usage:

```text
activate-profiles [profile1] [profile2] ...
```

Source: [David Wilson's config](https://github.com/daviwil/dotfiles/blob/master/Systems.org#activating-profiles)

```bash
GREEN='\033[1;32m'
RED='\033[1;30m'
NC='\033[0m'
GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles

profiles=$*
if [[ $# -eq 0 ]]; then
    profiles="$HOME/.config/guix/manifests/*.scm";
fi

for profile in $profiles; do
  # Remove the path and file extension, if any
  profileName=$(basename $profile)
  profileName="${profileName%.*}"
  profilePath="$GUIX_EXTRA_PROFILES/$profileName"
  manifestPath=$HOME/.config/guix/manifests/$profileName.scm

  if [ -f $manifestPath ]; then
    echo
    echo -e "${GREEN}Activating profile:" $manifestPath "${NC}"
    echo

    mkdir -p $profilePath
    guix package --manifest=$manifestPath --profile="$profilePath/$profileName"

    # Source the new profile
    GUIX_PROFILE="$profilePath/$profileName"
    if [ -f $GUIX_PROFILE/etc/profile ]; then
	. "$GUIX_PROFILE"/etc/profile
    else
	echo -e "${RED}Couldn't find profile:" $GUIX_PROFILE/etc/profile "${NC}"
    fi
  else
    echo "No profile found at path" $profilePath
  fi
done
```


### Update profiles {#update-profiles}

A script to update Guix profiles. Usage:

```text
update-profiles [profile1] [profile2] ...
```

Source: once again, [David Wilson's config](https://github.com/daviwil/dotfiles/blob/master/Systems.org#updating-profiles).

```bash
GREEN='\033[1;32m'
NC='\033[0m'
GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles

profiles=$*
if [[ $# -eq 0 ]]; then
    profiles="$GUIX_EXTRA_PROFILES/*";
fi

for profile in $profiles; do
  profileName=$(basename $profile)
  profilePath=$GUIX_EXTRA_PROFILES/$profileName

  echo
  echo -e "${GREEN}Updating profile:" $profilePath "${NC}"
  echo

  guix package --profile="$profilePath/$profileName" --manifest="$HOME/.config/guix/manifests/$profileName.scm"
done
```


## Channels {#channels}

Specifying additional channels.

[channel-q](https://github.com/SqrtMinusOne/channel-q) is my Guix channel. Don't use it at home.

References:

-   [nonguix channel repo](https://gitlab.com/nonguix/nonguix)
-   [Guix channels reference](https://guix.gnu.org/manual/en/html%5Fnode/Channels.html)

<!--listend-->

```scheme
(cons*
 (channel
  (name 'channel-q)
  (url "file:///home/pavel/Code/channel-q"))
 (channel
  (name 'flat)
  (url "https://github.com/flatwhatson/guix-channel.git")
  (introduction
   (make-channel-introduction
    "33f86a4b48205c0dc19d7c036c85393f0766f806"
    (openpgp-fingerprint
     "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  ;; (commit "d54973e47b89fe5772a5b6e2d0c0b86acb089e27")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
 %default-channels)
```


## Systems {#systems}

Configuring systems with Guix.

Yes, all my machines are named after colors I like.


### Base configuration {#base-configuration}

The base configuration is shared between all the machines.

While it's possible to make a single `.scm` file with base configuration and load it, I noticed that it produces more cryptic error messages whenever there is an error in the base file, so I opt-in for noweb.

`guix system` invocation is as follows:

```text
sudo -E guix system reconfigure ~/.config/guix/systems/[system].scm
```

Common modules:

```scheme
(use-modules (gnu))
(use-modules (gnu system nss))
(use-modules (gnu packages bash))
(use-modules ((gnu packages base) #:select (coreutils glibc)))
(use-modules (gnu packages certs))
(use-modules (gnu packages version-control))
(use-modules (gnu packages vim))
(use-modules (gnu packages gnome))
(use-modules (gnu packages xorg))
(use-modules (gnu packages wm))
(use-modules (gnu packages openbox))
(use-modules (gnu services docker))
(use-modules (gnu services cups))
(use-modules (gnu services virtualization))
(use-modules (srfi srfi-1))
(use-modules (guix channels))
(use-modules (guix inferior))
(use-modules (nongnu packages linux))
(use-modules (nongnu system linux-initrd))

(use-service-modules desktop networking ssh xorg nix)
(use-package-modules ssh)
```

In principle, we could define a variable called `base-operating-system` and extend it in ancestors. However, then we would have to define mandatory fields like `host-name`, `bootloader` with dummy values. Since I'm already using noweb, there is little point.

The following code will be inserted at the top of the `operating-system` definition.

Use the full Linux kernel. I hope I'll be able to use Libre kernel somewhere later.

Inferior in the kernel is used to avoid recompilation. It looks like I can pin these to different commits than in my `channels.scm`

```scheme
(kernel
 (let*
     ((channels
       (list (channel
	      (name 'nonguix)
	      (url "https://gitlab.com/nonguix/nonguix")
	      (commit "d3c5eea0cbfe3e5bfbcf1fe15bc916fefacc624f"))
	     (channel
	      (name 'guix)
	      (url "https://git.savannah.gnu.org/git/guix.git")
	      (commit "cf88c967afbf15c58efb0ba37d6638f1be9a0481"))))
      (inferior
       (inferior-for-channels channels)))
   (first (lookup-inferior-packages inferior "linux" "5.12.9"))))
;; (kernel linux)
(initrd microcode-initrd)
(firmware (list linux-firmware))
(locale "en_US.utf8")
(timezone "Europe/Moscow")
```

US/RU keyboard layout, switch with Alt+Shift.

```scheme
(keyboard-layout (keyboard-layout "us,ru" #:options '("grp:alt_shift_toggle")))
```

User accounts.

```scheme
(users (cons* (user-account
	       (name "pavel")
	       (comment "Pavel")
	       (group "users")
	       (home-directory "/home/pavel")
	       (supplementary-groups
		'("wheel"  ;; sudo
		  "netdev" ;; network devices
		  "audio"
		  "video"
		  "input"
		  "tty"
		  "docker"
		  "scanner"
		  "libvirt"
		  "lp")))
	      %base-user-accounts))

```

Base packages, necessary right after the installation.

```scheme
(packages
 (append
  (list nss-certs
	    git
	i3-gaps
	i3lock
	openbox
	xterm
	    vim)
  %base-packages))
```

Default services for each machine:

-   override the default `%desktop-services` to add OpenVPN support
-   add nix service
-   add docker service
-   add CUPS service
-   add libvirt service
-   add a symlink to ELF interpreter to where most Linux binaries expect it

<!--listend-->

```scheme
(define %my-base-services
  (cons*
   (service openssh-service-type)
   (screen-locker-service i3lock "i3lock")
   (extra-special-file "/lib64/ld-linux-x86-64.so.2" (file-append glibc "/lib/ld-linux-x86-64.so.2"))
   (service nix-service-type)
   (service cups-service-type
	    (cups-configuration
	     (web-interface? #t)))
   (service docker-service-type)
   (service libvirt-service-type
	    (libvirt-configuration
	     (unix-sock-group "libvirt")
	     (tls-port "16555")))
   (service virtlog-service-type)
   (modify-services %desktop-services
		    (network-manager-service-type
		     config =>
		     (network-manager-configuration
		      (inherit config)
		      (vpn-plugins (list network-manager-openvpn)))))))

```


### indigo {#indigo}

`indigo` is my desktop PC.

```scheme
<<system-common>>

(operating-system
 <<system-base>>

 (host-name "indigo")
 (services (cons*
	    (set-xorg-configuration
	     (xorg-configuration
	      (keyboard-layout keyboard-layout)))
	    %my-base-services))

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (keyboard-layout keyboard-layout)))

 (swap-devices
  (list (uuid "059a2c26-8f70-4986-adf0-1a2e7b511404")))

 (file-systems
  (cons* (file-system
	  (mount-point "/")
	  (device (file-system-label "my-root"))
	  (type "ext4"))
	     (file-system
	      (mount-point "/boot/efi")
	      (device "/dev/sda1")
	      (type "vfat"))
	 %base-file-systems)))
```


### eminence {#eminence}

`eminence` is a HP 15s laptop.

`%backlight-udev-rule` should enable members of `video` group change the display backlight. See the relevant page at [Arch Wiki](https://wiki.archlinux.org/title/Backlight).

```scheme
<<system-common>>

(define %backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
		  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
		  "\n"
		  "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
		  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(operating-system
 <<system-base>>

 (host-name "eminence")
 (services (cons*
	    (set-xorg-configuration
	     (xorg-configuration
	      (keyboard-layout keyboard-layout)))
	    (modify-services %my-base-services
			     (elogind-service-type
			      config =>
			      (elogind-configuration
			       (inherit config)
			       (handle-lid-switch-external-power 'suspend)))
			     (udev-service-type
			      config =>
			      (udev-configuration
			       (inherit config)
			       (rules (cons %backlight-udev-rule
					    (udev-configuration-rules config))))))))

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (keyboard-layout keyboard-layout)))

 (swap-devices
  (list (uuid "f93cf3f6-7ee7-42ec-8ee2-f3d896fdf9b5")))

 (file-systems
  (cons* (file-system
	  (mount-point "/")
	  (device
	   (uuid "1d937704-bbeb-43b5-bc63-453886c426af"
		 'ext4))
	  (type "ext4"))
	 (file-system
	  (mount-point "/boot/efi")
	  (device (uuid "0031-3784" 'fat32))
	  (type "vfat"))
	 %base-file-systems)))
```


### azure {#azure}

`azure` is a Lenovo Ideapad 330 laptop.

`%backlight-udev-rule` should enable members of `video` group change the display backlight. See the relevant page at [Arch Wiki](https://wiki.archlinux.org/title/Backlight).

```scheme
<<system-common>>

(define %backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
		  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
		  "\n"
		  "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
		  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(operating-system
 <<system-base>>

 (host-name "azure")
 (services (cons*
	    (set-xorg-configuration
	     (xorg-configuration
	      (keyboard-layout keyboard-layout)))
	    (modify-services %my-base-services
			     (elogind-service-type config =>
						   (elogind-configuration (inherit config)
									  (handle-lid-switch-external-power 'suspend)))
			     (udev-service-type config =>
						(udev-configuration (inherit config)
								    (rules (cons %backlight-udev-rule
										 (udev-configuration-rules config))))))))

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (keyboard-layout keyboard-layout)))

 (swap-devices
  (list (uuid "4b2dedb3-b111-4e69-8c05-6daa2b072c76")))

 (file-systems
  (cons* (file-system
	  (mount-point "/")
	  (device (file-system-label "my-root"))
	  (type "ext4"))
	     (file-system
	      (mount-point "/boot/efi")
	      (device "/dev/sda1")
	      (type "vfat"))
	 %base-file-systems)))
```


## System installation {#system-installation}


### Preparation {#preparation}

In my case, the provided ISO doesn't work because of the Libre kernel.

Fortunately, David Wilson has made [a repository](https://github.com/SystemCrafters/guix-installer) with a toolchain to make an ISO with the full kernel. In case it won't be an option, the [nonguix repo](https://gitlab.com/nonguix/nonguix) also has instructions on how to do that.

When an ISO is there, we have to write it on a USB stick. Run `sudo fdisk -l` to get a list of disks.

The approach given in the official instruction is to create a bootable USB with `dd`:

```text
sudo dd of=/dev/sdxX if=<path-to-iso> status=progress && sync
```

However, I couldn't make it work for some strange reason. Fortunately, `gnome-disk-utility` was able to produce a bootable USB.


### Installation {#installation}

Going further, the official instructions for installation & SystemCrafters wiki entry are pretty good, so it's not necessary to repeat them here.


### After installation {#after-installation}

After the installation, the strategy is as follows.

Set a password for the main user (pavel). Login with openbox to get a tolerable interface because i3's default config is horrible.

[Connect to the internet](https://guix.gnu.org/en/manual/en/html%5Fnode/Keyboard-Layout-and-Networking-and-Partitioning.html#Keyboard-Layout-and-Networking-and-Partitioning).

Clone the dotfiles repo:

```text
mkdir Code
cd Code
git clone https://github.com/SqrtMinusOne/dotfiles.git
```

Copy the channels file and run guix pull:

```text
cp ~/Code/dotfiles/.config/guix/channels.scm ~/.config/guix
guix pull
```

The first pull usually takes a while. After that install yadm and pull dotfiles:

```text
guix install yadm
guix clone https://github.com/SqrtMinusOne/dotfiles.git
```

And activate the required profiles. Again, downloading & building Emacs, Starship and stuff will take a while.

Don't forget to install `JetBrainsMono Nerd Font`.


## Misc software & notes {#misc-software-and-notes}

| Category | Guix dependency | Description                                        |
|----------|-----------------|----------------------------------------------------|
| system   | patchelf        | A program to modify existsing ELF executables      |
| system   | glibc           | A lot of stuff, including ELF interpeter and `ldd` |


### VPN {#vpn}

| Category | Guix dependency             |
|----------|-----------------------------|
| system   | openvpn                     |
| system   | openvpn-update-resolve-conf |
| system   | vpnc                        |

I'm not sure how to properly spin up VPN on Guix, so here is what ended I'm doing after some trial and error.

I'm using CyberGhost VPN. `~/.vpn` folder stores its OpenVPN config (`openvpn.ovpn`), modified as follows:

-   paths to `ca`, `cert` and `key` are made absolute

    ```vim
    ca /home/pavel/.vpn/ca.crt
    cert /home/pavel/.vpn/client.crt
    key /home/pavel/.vpn/client.key
    ```
-   added `auth-user-pass` with a link to login info

    ```vim
    auth-user-pass /home/pavel/.vpn/auth.conf
    ```

    `auth.conf` looks like this:

    ```text
    login
    password
    ```
-   run [openvpn-update-resolv-conf](https://github.com/alfredopalhares/openvpn-update-resolv-conf) script to fix DNS. `openvpn-update-resolve-conf` originates in my [channel-q](https://github.com/SqrtMinusOne/channel-q).

    ```vim
    setenv PATH /home/pavel/.guix-extra-profiles/system/system/bin:/home/pavel/.guix-extra-profiles/system/system/sbin:/home/pavel/.guix-extra-profiles/console/console/bin:/run/current-system/profile/bin:/run/current-system/profile/sbin

    up /home/pavel/.guix-extra-profiles/system/system/bin/update-resolv-conf.sh
    down /home/pavel/.guix-extra-profiles/system/system/bin/update-resolv-conf.sh
    ```

    `setenv PATH` is necessary because both `resolvconf` (openresolve) and `update-resolv-conf.sh` are shell scripts which need GNU coreutils and stuff, and OpenVPN clears PATH by default.
-   run a script to fix Docker routes

    ```vim
    route-up /home/pavel/bin/scripts/vpn-fix-routes
    ```

    References:

    -   [Github issue](https://github.com/moby/libnetwork/issues/779)

    The script itself:

    ```sh
    echo "Adding default route to $route_vpn_gateway with /0 mask..."

    IP=/run/current-system/profile/sbin/ip

    $IP route add default via $route_vpn_gateway

    echo "Removing /1 routes..."
    $IP route del 0.0.0.0/1 via $route_vpn_gateway
    $IP route del 128.0.0.0/1 via $route_vpn_gateway
    ```


#### vpn-start {#vpn-start}

As of now, CyberGhost doesn't provide ipv6, so we have to disable it.

```bash
export DISPLAY=:0
CONN=$(nmcli -f NAME con show --active | grep -Ev "(.*docker.*|NAME|br-.*|veth.*|tun.*|vnet.*|virbr.*)" | sed 's/ *$//g')

if [ -z "$CONN" ]; then
    echo "No connection!"
    notify-send "VPN" "No connection for VPN to run"
    exit
fi

echo "Connection: $CONN"
notify-send "VPN" "Initializing for connection: $CONN"

pkexec nmcli con modify "$CONN" ipv6.method ignore
nmcli connection up "$CONN"
pkexec openvpn --config ~/.vpn/openvpn.ovpn
```


#### vpn-stop {#vpn-stop}

Also a script to reverse the changes.

```bash
CONN=$(nmcli -f NAME con show --active | grep -Ev "(.*docker.*|NAME|br-.*|veth.*|tun.*)" | sed 's/ *$//g')
echo "Connection: $CONN"

pkexec nmcli con modify "$CONN" ipv6.method auto
nmcli connection up "$CONN"
```


### flatpak {#flatpak}

As for now, the easiest way to install most of proprietary software is via flatpak. See the relevant section in [Desktop.org]({{< relref "Desktop" >}}).


### conda {#conda}

[conda](https://docs.conda.io/en/latest/) is a package manager, which I use for managing various versions of Python & Node.js.

It is packaged for GNU Guix, although the definition has its fair share of workarounds. It is almost surprising to see it work with all the C libraries and stuff. But there are still some problems.

First, it's impossible to perform `conda init` to patch files like `.bashrc`, because the command is hell-bent on modifying `/gnu/store/`. So I do this manually, look for the `init_conda` procedures in [Console.org]({{< relref "Console" >}}).

Second, the base environment has `/gnu/store/...` as a root, so don't install anything there (and don't run `conda` with superuser rights!).

Third, by default it tries to create envronments in `/gnu/store`. It's enough to create one environment like this to fix it:

```sh
mkdir -p ~/.conda/envs
conda create -p ~/.conda/envs/test
```

Fourth, you may need to unset `$PYTHONPATH` if you have any global packages installed, otherwise Python from anaconda will try to import them instead of the conda versions.

Finally, I also want to have an ability to use global npm. Some settings for that are located in [Console.org](Console). Here we want to unset `NPM_CONFIG_USERCONFIG` if there is npm available in the environment.

So here is a script to set up conda hooks:

```bash
# Get writable conda envs with npm & without it
readarray -t CONDA_ENVS_ALL <<< $(conda env list --json | jq '.envs[]')
CONDA_ENVS_NPM=()
CONDA_ENVS_NO_NPM=()
for env in "${CONDA_ENVS_ALL[@]}"; do
    env="${env:1:${#env}-2}"
    if [ -w "$env" ]; then
	if [ -f "$env/bin/npm" ]; then
	    CONDA_ENVS_NPM+=($env)
	else
	    CONDA_ENVS_NO_NPM+=($env)
	fi
    fi
done

for env in "${CONDA_ENVS_NPM[@]}"; do
    echo "Found npm in $env"
    mkdir -p "$env/etc/conda/activate.d"
    mkdir -p "$env/etc/conda/deactivate.d"

    echo "unset NPM_CONFIG_USERCONFIG" > "$env/etc/conda/activate.d/conda.sh"
    echo "set -e NPM_CONFIG_USERCONFIG" > "$env/etc/conda/activate.d/conda.fish"
    echo "export NPM_CONFIG_USERCONFIG=$HOME/._npmrc" > "$env/etc/conda/deactivate.d/conda.sh"
    echo "export NPM_CONFIG_USERCONFIG=$HOME/._npmrc" > "$env/etc/conda/deactivate.d/conda.fish"
done

for env in "${CONDA_ENVS_NO_NPM}"; do
    echo "Did not found npm in $env"
    rm -rf "$env/etc/conda/activate.d/conda.sh" || true
    rm -rf "$env/etc/conda/activate.d/conda.fish" || true
    rm -rf "$env/etc/conda/deactivate.d/conda.sh" || true
    rm -rf "$env/etc/conda/deactivate.d/conda.fish" || true
done
```


### Slack {#slack}

What a nonsense of a program.

I was able to launch the nix version with the following wrapper script:

```bash
export PATH="$HOME/bin/dummies:$PATH"
mkdir -p ~/.cache/slack
slack -r ~/.cache/slack
```

Also, it requires a `lsb_release` in the PATH, so here is one:

```bash
echo "LSB Version:    Hey. I spent an hour figuring out why Slack doesn't launch."
echo "Distributor ID: It seems like it requires an lsb_release."
echo "Description:    But GNU Guix doesn't have one."
echo "Release:        42.2"
echo "Codename:       n/a"
```


### virt-manager {#virt-manager}

Run the following to fix the network:

```sh
sudo virsh net-define /run/current-system/profile/etc/libvirt/qemu/networks/default.xml
sudo virsh net-start default
sudo herd restart libvirtd
```


### wakatime-cli {#wakatime-cli}

| Note | Description           |
|------|-----------------------|
| TODO | Package this for Guix |

Before I figure out how to package this for Guix:

-   Clone [the repo](https://github.com/wakatime/wakatime-cli)
-   Run `go build`
-   Copy the binary to the `~/bin` folder


### Manifest {#manifest}

<a id="code-snippet--packages"></a>
```emacs-lisp
(my/format-guix-dependencies category)
```

System

```scheme
(specifications->manifest
 '(
   <<packages("system")>>))
```