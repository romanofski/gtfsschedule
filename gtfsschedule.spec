# https://fedoraproject.org/wiki/Packaging:Haskell

%global pkg_name gtfsschedule
%global pkgver %{pkg_name}-%{version}

%bcond_with tests
%global with_tests 1

Name:           %{pkg_name}
Version:        0.8.3.0
Release:        2%{?dist}
Summary:        Be on time for your next public transport service

License:        GPLv3+
Url:            https://github.com/romanofski/%{name}
Source0:        https://github.com/romanofski/%{name}/archive/%{version}.tar.gz

BuildRequires:  ghc-Cabal-devel
BuildRequires:  ghc-rpm-macros
# Begin cabal-rpm deps:
BuildRequires:  chrpath
BuildRequires:  asciidoc
%if 0%{?fedora} < 31
BuildRequires:  ghc-bytestring-devel
BuildRequires:  ghc-cassava-devel
BuildRequires:  ghc-conduit-devel
BuildRequires:  ghc-conduit-extra-devel
BuildRequires:  ghc-containers-devel
BuildRequires:  ghc-directory-devel
BuildRequires:  ghc-esqueleto-devel
BuildRequires:  ghc-http-conduit-devel
BuildRequires:  ghc-http-types-devel
BuildRequires:  ghc-monad-logger-devel
BuildRequires:  ghc-mtl-devel
BuildRequires:  ghc-old-locale-devel
BuildRequires:  ghc-optparse-applicative-devel
BuildRequires:  ghc-persistent-devel
BuildRequires:  ghc-persistent-sqlite-devel
BuildRequires:  ghc-persistent-template-devel
BuildRequires:  ghc-protocol-buffers-devel
BuildRequires:  ghc-resourcet-devel
BuildRequires:  ghc-system-filepath-devel
BuildRequires:  ghc-temporary-devel
BuildRequires:  ghc-text-devel
BuildRequires:  ghc-time-devel
BuildRequires:  ghc-transformers-devel
BuildRequires:  ghc-utf8-string-devel
BuildRequires:  ghc-xdg-basedir-devel
BuildRequires:  ghc-zip-archive-devel
BuildRequires:  ghc-ini-devel
BuildRequires:  ghc-HStringTemplate-devel
BuildRequires:  ghc-bifunctors-devel
%else
BuildRequires:  ghc-HStringTemplate-prof
BuildRequires:  ghc-base-prof
BuildRequires:  ghc-bytestring-prof
BuildRequires:  ghc-cassava-prof
BuildRequires:  ghc-conduit-prof
BuildRequires:  ghc-conduit-extra-prof
BuildRequires:  ghc-containers-prof
BuildRequires:  ghc-directory-prof
BuildRequires:  ghc-esqueleto-prof
BuildRequires:  ghc-http-client-prof
BuildRequires:  ghc-http-conduit-prof
BuildRequires:  ghc-http-types-prof
BuildRequires:  ghc-ini-prof
BuildRequires:  ghc-monad-control-prof
BuildRequires:  ghc-monad-logger-prof
BuildRequires:  ghc-mtl-prof
BuildRequires:  ghc-old-locale-prof
BuildRequires:  ghc-optparse-applicative-prof
BuildRequires:  ghc-persistent-prof
BuildRequires:  ghc-persistent-sqlite-prof
BuildRequires:  ghc-persistent-template-prof
BuildRequires:  ghc-protocol-buffers-prof
BuildRequires:  ghc-resourcet-prof
BuildRequires:  ghc-system-filepath-prof
BuildRequires:  ghc-temporary-prof
BuildRequires:  ghc-text-prof
BuildRequires:  ghc-time-prof
BuildRequires:  ghc-transformers-prof
BuildRequires:  ghc-unliftio-core-prof
BuildRequires:  ghc-utf8-string-prof
BuildRequires:  ghc-xdg-basedir-prof
BuildRequires:  ghc-zip-archive-prof
%endif

%if %{with tests}
BuildRequires:  ghc-lifted-base-devel
BuildRequires:  ghc-network-devel
BuildRequires:  ghc-streaming-commons-devel
BuildRequires:  ghc-tasty-devel
BuildRequires:  ghc-tasty-hunit-devel
BuildRequires:  ghc-tasty-quickcheck-devel
BuildRequires:  ghc-transformers-base-devel
%endif
# End cabal-rpm deps

%description
Please see README.adoc.


%package -n ghc-%{name}
Summary:        Haskell %{name} library

%description -n ghc-%{name}
This package provides the Haskell %{name} shared library.


%package -n ghc-%{name}-devel
Summary:        Haskell %{name} library development files
Provides:       ghc-%{name}-static = %{version}-%{release}
Provides:       ghc-%{name}-static%{?_isa} = %{version}-%{release}
%if %{defined ghc_version}
Requires:       ghc-compiler = %{ghc_version}
%endif
Requires:       ghc-%{name}%{?_isa} = %{version}-%{release}

%description -n ghc-%{name}-devel
This package provides the Haskell %{name} library development files.


%if %{with haddock}
%package -n ghc-%{name}-doc
Summary:        Haskell %{name} library documentation

%description -n ghc-%{name}-doc
This package provides the Haskell %{name} library documentation.
%endif


%if %{with ghc_prof}
%package -n ghc-%{name}-prof
Summary:        Haskell %{name} profiling library
Requires:       ghc-%{name}-devel%{?_isa} = %{version}-%{release}

%description -n ghc-%{name}-prof
This package provides the Haskell %{name} profiling library.
%endif

%prep
%setup -q -n %{pkgver}


%build
%ghc_lib_build
make man/gtfsschedule.1


%install
%ghc_lib_install

rm %{buildroot}/%{?_defaultlicensedir}%{!?_defaultlicensedir:%_docdir}/%{name}/LICENSE

install -m 0644 -p -D man/gtfsschedule.1 %{buildroot}%{_mandir}/man1/gtfsschedule.1


%check
%if %{with tests}
%cabal test
%endif


%post -n ghc-%{name}-devel
%ghc_pkg_recache


%postun -n ghc-%{name}-devel
%ghc_pkg_recache


%files
# Begin cabal-rpm files:
%license LICENSE
%doc ChangeLog.adoc README.adoc
%{_bindir}/%{name}
%{_mandir}/man1/gtfsschedule.1*
# End cabal-rpm files


%files -n ghc-%{name} -f ghc-%{name}.files
# Begin cabal-rpm files:
%license LICENSE
# End cabal-rpm files


%files -n ghc-%{name}-devel -f ghc-%{name}-devel.files
%doc ChangeLog.adoc README.adoc


%if %{with haddock}
%files -n ghc-%{name}-doc -f ghc-%{name}-doc.files
%endif


%if %{with ghc_prof}
%files -n ghc-%{name}-prof -f ghc-%{name}-prof.files
%endif


%changelog
* Tue Nov 05 2019 Róman Joost <roman@bromeco.de> - 0.8.2.0-1
- rebuild for F31

* Sat Nov 17 2018 Róman Joost <roman@bromeco.de> - 0.8.1.0-1
- 0.8.1 release

* Sat Apr 1 2017 Róman Joost <roman@bromeco.de> - 0.8.0.0-1
- 0.8 release

* Thu Feb 23 2017 Róman Joost <roman@bromeco.de> - 0.7.0.0-1
- 0.7 release

* Sun Feb 05 2017 Róman Joost <roman@bromeco.de> - 0.6.0.0-1
- 0.6 release

* Wed Jan 11 2017 Róman Joost <roman@bromeco.de> - 0.5.0.0-1
- 0.5 release

* Mon Dec 12 2016 Róman Joost <roman@bromeco.de> - 0.4.0.0-1.20161212
- nightly build with fix for #1

* Sun Dec 04 2016 Róman Joost <roman@bromeco.de> - 0.4.0.0-1.20161204
- use a different macro to remove the license from the buildroot during
  build

* Mon Nov 28 2016 Róman Joost <roman@bromeco.de> - 0.4.0.0-1.20161127
- GHC 7.10.3 upgrade

* Thu Nov 17 2016 Róman Joost <roman@bromeco.de> - 0.4.0.0-1
- 0.4 release

* Tue Nov 15 2016 Fedora Haskell SIG <haskell@lists.fedoraproject.org> - 0.3.1.0-0.20161115
- spec file generated by cabal-rpm-0.9.10
