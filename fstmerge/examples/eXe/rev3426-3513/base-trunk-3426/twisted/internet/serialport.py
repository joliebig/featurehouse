"""
Serial Port Protocol
"""
import os, sys
import serial
from serial import PARITY_NONE, PARITY_EVEN, PARITY_ODD
from serial import STOPBITS_ONE, STOPBITS_TWO
from serial import FIVEBITS, SIXBITS, SEVENBITS, EIGHTBITS
class BaseSerialPort:
    def setBaudRate(self, baudrate):
        if hasattr(self._serial, "setBaudrate"):
            self._serial.setBaudrate(baudrate)
        else:
            self._serial.setBaudRate(baudrate)
    def inWaiting(self):
        return self._serial.inWaiting()
    def flushInput(self):
        self._serial.flushInput()
    def flushOutput(self):
        self._serial.flushOutput()
    def sendBreak(self):
        self._serial.sendBreak()
    def getDSR(self):
        return self._serial.getDSR()
    def getCD(self):
        return self._serial.getCD()
    def getRI(self):
        return self._serial.getRI()
    def getCTS(self):
        return self._serial.getCTS()
    def setDTR(self, on = 1):
        self._serial.setDTR(on)
    def setRTS(self, on = 1):
        self._serial.setRTS(on)
class SerialPort(BaseSerialPort):
    pass
if os.name == 'posix':
    from twisted.internet._posixserialport import SerialPort
elif os.name == 'java':
    from twisted.internet._javaserialport import SerialPort
elif sys.platform == 'win32':
    from twisted.internet._win32serialport import SerialPort
