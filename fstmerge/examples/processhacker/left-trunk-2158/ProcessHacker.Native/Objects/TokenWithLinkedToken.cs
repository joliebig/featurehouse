

using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;
using System;

namespace ProcessHacker.Native.Objects
{
    public sealed class TokenWithLinkedToken : IWithToken
    {
        private TokenHandle _token;

        public TokenWithLinkedToken(TokenHandle token)
        {
            _token = token;
        }

        public TokenHandle GetToken()
        {
            IntPtr linkedToken;
            int retLen;

            if (!Win32.GetTokenInformation(_token, TokenInformationClass.TokenLinkedToken,
                out linkedToken, IntPtr.Size, out retLen))
                Win32.ThrowLastError();

            return new TokenHandle(linkedToken, true);
        }

        public TokenHandle GetToken(TokenAccess access)
        {
            return this.GetToken();
        }
    }
}
