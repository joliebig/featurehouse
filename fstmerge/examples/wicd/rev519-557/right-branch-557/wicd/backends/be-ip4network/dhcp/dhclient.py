import os
from basedhcp import BaseDhcpClient
import misc
from logfile import log
from globalconfig import global_config
class DhcpDhclient(BaseDhcpClient):
    def __init__(self, interface_name):
        BaseDhcpClient.__init__(self, interface_name)
        self.dhclient_path = self._find_dhclient_binary()
    @staticmethod
    def _find_dhclient_binary():
        return misc.find_program_in_path('dhclient')
    @staticmethod
    def check():
        ''' Check to make sure the required utilties exist
        Searchs $PATH for the dhclient binary.
        Keyword arguments:
        None
        Returns:
        True if all required dhclient binaries are found, False otherwise.
        '''
        return bool(DhcpDhclient._find_dhclient_binary())
    def _parse_dhclient(self, pipe):
        """ Parse the output of dhclient.
        Parses the output of dhclient and returns the status of
        the connection attempt.
        Keyword arguments:
        pipe -- stdout pipe to the dhcpcd process.
        Returns:
        'success' if succesful', an error code string otherwise.
        """
        dhclient_complete = False
        dhclient_success = False
        print_dhclient_output = global_config.get('dhclient', 'print_output', 
                                                  True)
        while not dhclient_complete:
            line = pipe.readline()
            if line == '':  
                dhclient_complete = True
            else:
                if print_dhclient_output: log(line.strip('\n'))
            if line.startswith('bound'):
                log(line.strip('\n'))
                dhclient_success = True
                dhclient_complete = True
        return dhclient_success
    def status(self):
        ''' Determines whether dhclient managed to obtain an IP address.
        Blocks while parses the output of a running dhclient and returns 
        the status. If method has already been run, returns the last status
        unless start has been run since.
        Keyword arguments:
        None
        Returns:
        True if IP is obtained, False otherwise.
        '''
        if not hasattr(self, '_last_status'):
            if not hasattr(self, 'dhclient'):
                raise misc.WicdError('Must run start() before status()')
            status = self._parse_dhclient(self.dhclient.stdout)
            self._last_status = status
            return status
        else:
            return self._last_status
    def start(self):
        self.stop()
        cmd = '%s -d %s' % (misc.find_program_in_path('dhclient'),
                            self.interface_name)
        self.dhclient = misc.run(cmd,
                                include_stderr=True,
                                return_fileobject=True)
    def stop(self):
        if hasattr(self, 'dhclient'):
            log('killing dhclient %s' % self.dhclient.pid)
            os.kill(self.dhclient.pid, 15)
        else:
            log('no dhclient running')
        if hasattr(self, '_last_status'): del self._last_status
    def __del__(self):
        self.stop()
