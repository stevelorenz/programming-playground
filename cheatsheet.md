# Cheat Sheet

- Copy (scp) file from host to the Vagrant VM using localhost IP

```bash
scp -i ./.vagrant/machines/playground/virtualbox/private_key -P 2222 ./cheatsheet.md vagrant@127.0.0.1:/home/vagrant/
```

