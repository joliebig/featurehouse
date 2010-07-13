

using System;
using System.Text;

namespace ProcessHacker.Common
{



    public static class BaseConverter
    {
        private static int[] _reverseChars = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 37, 38, 39, 40, 41,
            42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 52,
            53, 54, 55, 56, 57, 58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 59, 60, 61, 62, 63, 64, 10, 11, 12, 13, 14,
            15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
            33, 34, 35, 65, 66, 67, 68, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };






        public static string ReverseString(string str)
        {
            StringBuilder sb = new StringBuilder();

            for (int i = str.Length - 1; i >= 0; i--)
            {
                sb.Append(str[i]);
            }

            return sb.ToString();
        }
        public static decimal ToNumber(string number, int b)
        {
            if (b > 70)
                return 0;
            if (number == "")
                return 0;
            bool negative = number[0] == '-';
            int length = number.Length;
            long result = 0;
            if (negative)
            {
                length -= 1;
            }
            number = ReverseString(number).ToLower();
            for (int i = 0; i < length; i++)
            {
                result += _reverseChars[number[i]] * ((long)Math.Pow(b, i));
            }
            if (negative)
                return -result;
            else
                return result;
        }
        public static decimal ToNumberParse(string number)
        {
            return ToNumberParse(number, true);
        }
        public static decimal ToNumberParse(string number, bool allowNonStandardExts)
        {
            if (number == "")
                return 0;
            bool negative = number[0] == '-';
            decimal result = 0;
            if (negative)
                number = number.Substring(1);
            if (number.Length > 2 && (number.Substring(0, 2) == "0x"))
            {
                result = ToNumber(number.Substring(2), 16);
            }
            else if (number.Length > 1)
            {
                if (number[0] == '0')
                {
                    result = ToNumber(number.Substring(1), 8);
                }
                else if (number[0] == 'b' && allowNonStandardExts)
                {
                    result = ToNumber(number.Substring(1), 2);
                }
                else if (number[0] == 't' && allowNonStandardExts)
                {
                    result = ToNumber(number.Substring(1), 3);
                }
                else if (number[0] == 'q' && allowNonStandardExts)
                {
                    result = ToNumber(number.Substring(1), 4);
                }
                else if (number[0] == 'w' && allowNonStandardExts)
                {
                    result = ToNumber(number.Substring(1), 12);
                }
                else if (number[0] == 'r' && allowNonStandardExts)
                {
                    result = ToNumber(number.Substring(1), 32);
                }
                else
                {
                    result = ToNumber(number, 10);
                }
            }
            else
            {
                result = ToNumber(number, 10);
            }
            if (negative)
                return -result;
            else
                return result;
        }
    }
}
