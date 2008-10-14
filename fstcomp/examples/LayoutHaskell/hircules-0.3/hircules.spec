# RPM spec file for hircules  -*-rpm-spec-*-
#
# Copyright 2003 Jens-Ulrik Petersen <petersen@haskell.org>

%define ghc_ver 6.0.1

Name: hircules
Summary: A IRC client in Haskell
Version: 0.3
Release: 1
License: Distributable
Group: Applications/Internet
Source: http://haskell.org/hircules/src/%{name}-%{version}.tar.gz
#Source1: http://prdownloads.sourceforge.net/haskell-libs/lambdabot-1.0.tar.gz
URL: http://haskell.org/hircules/
Packager: Jens Petersen <petersen@haskell.org>
BuildRoot: %{_tmppath}/%{name}-%{version}-root
BuildRequires: ghc5 gtk2hs-ghc%{ghc_ver}
Requires: gtk2

%description
Hircules is an IRC client written in Haskell, using gtk2hs for the UI.
The IRC library is based on lambdabot.

# the debuginfo subpackage is currently empty anyway, so don't generate it
%define debug_package %{nil}
%define __spec_install_post /usr/lib/rpm/brp-compress

%prep
%setup -q
#%%setup -q -a1

%build
./configure --prefix=%{_prefix} --with-hc=ghc-%{ghc_ver} # --with-lambdabot=lambdabot
make

%install
%makeinstall

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc ChangeLog INSTALL README NEWS AUTHORS COPYING index.html
%{_prefix}/bin/*

%changelog
* Sun Oct  5 2003 Jens Petersen <petersen@redhat.com> - 0.3-1
- build without lambdabot support

* Thu Jul  3 2003 Jens Petersen <petersen@redhat.com> - 0.2-1
- update to 0.2

* Wed May 14 2003 Jens Petersen <petersen@redhat.com> - 0.1-1
- initial packaging
