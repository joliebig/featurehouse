"""Deprecated module that used to contain SelectReactor and PosixReactorBase
API Stability: stable
Maintainer: U{Itamar Shtull-Trauring<mailto:twisted@itamarst.org>}
"""
import warnings
warnings.warn("twisted.internet.default is deprecated. Use posixbase or selectreactor instead.", category=DeprecationWarning)
from posixbase import PosixReactorBase
from selectreactor import SelectReactor, install
__all__ = ["install", "PosixReactorBase", "SelectReactor"]
