# Virtual machine

This folder contains scripts for setting up a virtual machine with LineageFlow for demos.

## Setting up SSH to the virtual machine

- [Download](https://nixos.org/nixos/download.html) the NixOS VirtualBox appliance;

- Import image into VirtualBox and run it;

- Login using the username *demo* and password *demo*;

- Run `ip addr` from the terminal and find the 10.0.2.* IP adress.

- Setup [port forwarding](http://blog.johannesmp.com/2017/01/25/port-forwarding-ssh-from-virtualbox/) for VirtualBox.
  In this case, I did it between 127.0.0.1 port 3022 to 10.0.2.15 port 22.

- Edit the nixos configuration with `nano /etc/nixos/configuration.nix` and add the line `services.sshd.enable = true;`

From this step on, you can SSH into the virtual machine.
In order to make it more easily, you can add the following lines to `~/.ssh/config`:

```
Host lineageflow
    HostName 127.0.0.1
    Port 3022
    User demo
```

which allows you to do `ssh lineageflow`.

## Setting up keys

The next step is to [setup your keys](https://github.com/NixOS/nix/blob/1.11.4/doc/signing.txt).
Supposing you already have them in `/etc/nix/`, move them to the virtual machine:

```
scp /etc/nix/signing-key.pub lineageflow:/home/demo/signing-key.pub
ssh lineageflow
sudo mv signing-key.pub /etc/nix
```

## Generating and transferring the server and executables

Just execute `./make-closure` from within this folder, which will generate an `out` symlink.
The packages that are going to be included in the VM can be changed by editing the `make-closure` file.

Finally, transfer the closure with `./transfer-closure`, which can take some time.

In order to intall the packages in the environment, execute the following in the virtual machine:

```
LF=$(ls /nix/store | grep lineageflow | egrep -v doc)
for P in $LF; do nix-env -i $P done
```

## Transferring the client

Make sure you built the client before, and execute `./transfer-client`.

## Transferring the database

Just execute `./transfer-database DATABASE_PATH`, replacing the path by the one you want to transfer.
