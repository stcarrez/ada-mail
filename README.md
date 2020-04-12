# Ada Mail

[![Build Status](https://img.shields.io/jenkins/s/https/jenkins.vacs.fr/Bionic-Ada-Mail.svg)](http://jenkins.vacs.fr/job/Bionic-Ada-Mail/)
[![Test Status](https://img.shields.io/jenkins/t/https/jenkins.vacs.fr/Bionic-Ada-Mail.svg)](http://jenkins.vacs.fr/job/Bionic-Ada-Mail/)
[![License](http://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-keystore/0.0.0.svg)
![semver](https://img.shields.io/badge/semver-2.0.0-blue.svg?cacheSeconds=2592000)

# TL;DR

MGREP is a tool to grep mail content while taking account the mail
headers, formats and encodings.

```
   mgrep <pattern> <file|directory>
```

# Overview

Ada Mail is library to parse mail content and perform useful operations
on mail content.  It also provide an interface to write
a [Milter](https://en.wikipedia.org/wiki/Milter) service that can be
plugged in [Postfix](http://www.postfix.org/) to implement specific
mail filters.

Ada Mail also provides the `mgrep` tool that allows to search for
patterns in mail content.

# Building Ada Mail

To build the Ada Mail you will need the GNAT Ada compiler, either
the FSF version available in Debian, FreeBSD systems NetBSD or the
AdaCore GNAT Community 2019 edition.  The following libraries are used:

* Ada Util (https://github.com/stcarrez/ada-util)
* XMLAda   (https://libre.adacore.com/libre/tools/xmlada/)

## Development Host Installation

### Ubuntu

Install the following packages:

```
sudo apt-get install -y make gnat-7 gprbuild git libxmlada-unicode7-dev
```

### FreeBSD 12

Install the following packages:

```
pkg install gmake gcc6-aux-20180516_1,1 gprbuild-20160609_1 git
```

### Windows

Get the Ada compiler from [AdaCore Download](https://www.adacore.com/download)
site and install.

Install the following packages:

```
pacman -S git
pacman -S make
pacman -S base-devel --needed
```

## Getting the sources

The project uses a sub-module to help you in the integration and build
process.  You should checkout the project with the following commands:

```
   git clone https://github.com/stcarrez/ada-mail.git
   cd ada-mail
```

## Configuration

To configure Ada Mail, use the following command:
```
   ./configure
```

## Build

Then, build the application:
```
   make
```

And install it:
```
   make install
```
