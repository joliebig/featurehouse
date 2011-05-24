import misc
from dhcpmanager import DhcpManager
from netutilsmanager import NetUtilsManager
import logging
manager = NetUtilsManager()
netutils = manager.get_netutils()
class NetworkInterface(netutils.NetworkInterface):
    ''' Represents a network interface. '''
    def __init__(self, interface_name):
        netutils.NetworkInterface.__init__(self, interface_name)
        self.dhcp_manager = DhcpManager(interface_name)
    def do_ip_address(self, profile):
        if profile.get('use_static_ip', False):
            logging.debug('do static ip')
            ip_addresses = ['static_ip', 'static_netmask', 'static_dns_1']
            optional = ['static_gateway']
            misc.validate_ips(profile, ['static_ip', 'static_netmask'])
            misc.validate_ips(profile, ['static_gateway'], optional=True)
            self.set_ip(profile['static_ip'])
            self.set_netmask(profile['static_netmask'])
            if 'static_gateway' in profile and profile['static_gateway']:
                self.set_gateway(profile['static_gateway'])
        else:
            logging.debug('using dhcp')
            tmp_profile = dict(profile)
            if profile.get('use_static_dns', False):
                tmp_profile['dhcp_get_domain-name-servers'] = False
            self.dhcp_manager.start(tmp_profile)
        if profile.get('use_static_dns', False):
            static_dns = dict( [ (key, value)
                           for key, value in profile
                           if key.startswith('static_dns_') ] )
            static_dns = [ ip[1] for ip in static_dns ]
            names = [ key
                      for key, value in profile
                      if key.startswith('static_dns_')]
            names.sort()
            misc.validate_ips(profile, names, optional=True)
            static_dns = [ profile.get(name) for name in names ]
            self.set_dns(static_dns)
        if profile.get('use_static_ip', False):
            return True
        else:
            return self.dhcp_manager.status()
    def disconnect(self):
        self.reset()
    def reset(self):
        self.dhcp_manager.stop()
        self.set_ip('0.0.0.0')
        self.down()
