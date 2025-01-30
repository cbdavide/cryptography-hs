# Cryptography HS

## Examples

**Encrypt**

```bash
$ cryptography-hs-exe aes-cipher -e -s 128 -k 1ccd12a46d184b7c5a9bdff876aac2f2 -n a127e79c082cd1c8ce00daa2186029d5 -i input.txt -o output.txt
```

**Decrypt**

```bash
$ cryptography-hs-exe aes-cipher -d -s 128 -k 1ccd12a46d184b7c5a9bdff876aac2f2 -n a127e79c082cd1c8ce00daa2186029d5 -i output.txt -o plain.txt
```

## External Dependencies

* Botan >= 3.0.0 (follow these [instructions](https://botan.randombit.net/handbook/building.html) to build from source)

```bash
$ ./configure.py --prefix=/usr/
$ make
$ make install
```

In case `pkg-config` doesn't find the botan-3 lib

```bssh
$ cd /usr/share/pkgconfig
$ sudo ln -s /usr/lib/pkgconfig/botan-3.pc .
```

Verify `pkg-config` is able to find botan-3 lib

```bash
$ pkg-config --list-all | rg botan
// List the directories pkg-config looks in by default
$ pkg-config --variable pc_path pkg-config
```

In case of error:

```bash
error while loading shared libraries: libbotan-3.so.6: cannot open shared object file: No such file or directory
```

Run the following command:

```bash
$ sudo ldconfig
```

Check that the compiled file can find the botan-3 shared object:

```bash
$ stack install
$ ldd ~/.local/bin/cryptography-hs-exe
```
