---
title: Setting an SSH Key's Password
---

This was a royal pain in the ass to figure out, so hopefully this'll
save you or someone else some time. What follows is pretty much copied
verbatim from the following site:\
<http://www.noah.org/wiki/SSH_public_keys>

**Note to Windows/msysgit users:** the following commands will work
without a hitch if you run them from the "GIT Bash". Otherwise, you'll
need to ensure that openssl is on your PATH. I'm assuming here that your
key is located at <code>%userprofile%/.ssh/id\_rsa</code>.

# Unencrypt your existing key

If your existing key is encrypted (read: has a password), you'll need to
remove the encryption first (this will obviously overwrite the existing
key file):

``` {.console}
openssl rsa -in ~/.ssh/id_rsa -out ~/.ssh/id_rsa
```

# Encrypt your existing key

Once you've unencrypted your key, you'll need to re-encrypt it (this
will obviously overwrite the existing key file):

``` {.console}
openssl rsa -des3 -in ~/.ssh/id_rsa -out ~/.ssh/id_rsa
```

When you run the above command, you'll be prompted for your new
password.
