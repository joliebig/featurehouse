

using System;
using ProcessHacker.Native.Api;

namespace ProcessHacker.Native
{
    public class WindowsException : Exception
    {
        private bool _isNtStatus = false;
        private Win32Error _errorCode = 0;
        private NtStatus _status;
        private string _message = null;
        public WindowsException()
        { }
        public WindowsException(Win32Error errorCode)
        {
            _errorCode = errorCode;
        }
        public WindowsException(NtStatus status)
        {
            _status = status;
            _errorCode = status.ToDosError();
            _isNtStatus = true;
        }
        public bool IsNtStatus
        {
            get { return _isNtStatus; }
        }
        public Win32Error ErrorCode
        {
            get { return _errorCode; }
        }
        public NtStatus Status
        {
            get { return _status; }
        }
        public override string Message
        {
            get
            {
                if (_message == null)
                {
                    if (
                        _isNtStatus &&
                        _status != NtStatus.AccessDenied &&
                        _status != NtStatus.AccessViolation
                        )
                    {
                        string message = _status.GetMessage();
                        if (message == null)
                            message = "Could not retrieve the error message (0x" + ((int)_status).ToString("x") + ").";
                        _message = message;
                    }
                    else
                    {
                        _message = _errorCode.GetMessage();
                    }
                }
                return _message;
            }
        }
    }
}
