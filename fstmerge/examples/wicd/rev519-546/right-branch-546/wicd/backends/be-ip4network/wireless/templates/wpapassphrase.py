from template import BaseTemplate, Requirement
class TemplateWpaPassphrase(BaseTemplate):
    name = "WPA 1/2 (Passphrase)"
    author = "Adam Blackburn"
    version = 1
    require = [ Requirement('psk', 'PSK', 'string') ]
    template = '''
ctrl_interface=/var/run/wpa_supplicant
network={
       ssid="$_ESSID"
       scan_ssid=$_SCAN
       proto=WPA RSN
       key_mgmt=WPA-PSK
       pairwise=CCMP TKIP
       group=CCMP TKIP
       psk="$_PSK"
}
'''
    @staticmethod
    def detect_encryption(): return False
