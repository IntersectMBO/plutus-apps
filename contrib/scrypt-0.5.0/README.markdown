# Welcome to scrypt

This is a Haskell library providing bindings to [Colin Percival's scrypt implementation](http://www.tarsnap.com/scrypt.html). Scrypt is a key derivation function designed to be far more secure against hardware brute-force attacks than alternative functions such as PBKDF2 or bcrypt.

Details of the scrypt key derivation function are given in a paper by Colin Percival, Stronger Key Derivation via Sequential Memory-Hard Functions: [PDF](http://www.tarsnap.com/scrypt/scrypt-slides.pdf).

# Join in!

We are happy to receive bug reports, fixes, documentation enhancements, and other improvements.

Please report bugs via the [github issue tracker](http://github.com/informatikr/scrypt/issues).

Master [git repository](http://github.com/informatikr/scrypt):

    git clone git://github.com/informatikr/scrypt.git

# Authors

This library is written and maintained by Falko Peters, <falko.peters@gmail.com>.

Thanks to Thomas DuBuisson for suggesting the changes to make this package windows-compatible.
