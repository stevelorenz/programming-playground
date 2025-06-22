# Programming Playground

> Übung macht Meister.

# Cheat Sheet

- Copy (scp) a file from the host to the Vagrant VM using the localhost IP

```bash
scp -i .vagrant/machines/playground/virtualbox/private_key -P 2222 ./README.md vagrant@127.0.0.1:/home/vagrant/
```
