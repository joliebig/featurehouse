

using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{







    public interface IWithToken
    {




        TokenHandle GetToken();






        TokenHandle GetToken(TokenAccess access);
    }
}
