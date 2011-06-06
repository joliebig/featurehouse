available = False # hack to deal with half-broken imports in python <2.4
from OpenSSL import SSL
import sslverify
from sslverify import DistinguishedName, KeyPair
peerFromTransport = sslverify.Certificate.peerFromTransport
from twisted.pb import base32
class MyOptions(sslverify.OpenSSLCertificateOptions):
    def _makeContext(self):
        ctx = sslverify.OpenSSLCertificateOptions._makeContext(self)
        def alwaysValidate(conn, cert, errno, depth, preverify_ok):
            things_are_ok = (0,  # X509_V_OK
                             18, # X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT
                             19, # X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN
                             )
            if errno in things_are_ok:
                return 1
            return 0
        ctx.set_verify(SSL.VERIFY_PEER |
                       SSL.VERIFY_CLIENT_ONCE,
                       alwaysValidate)
        return ctx
def digest32(colondigest):
    digest = "".join([chr(int(c,16)) for c in colondigest.split(":")])
    digest = base32.encode(digest)
    return digest
available = True
