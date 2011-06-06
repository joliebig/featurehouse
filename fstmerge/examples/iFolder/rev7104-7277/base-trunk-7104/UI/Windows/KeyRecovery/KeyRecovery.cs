using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.Text.RegularExpressions;
using System.IO;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using Novell.iFolder.Utility;
namespace Novell.iFolder.Utility
{
    class KeyRecovery
    {
        public RSACryptoServiceProvider rsadec;
        private string titleTag = "CryptoKeyRecovery";
        private string CollectionIDTag = "iFolderCollection";
        private string iFolderIDTag = "iFolderID";
        private string KeyTag = "Key";
        public Option keyPath = new Option("key-path,kp", "Private Key", "Path to the Private key file (.p12 format)", true, null);
        public Option inpath = new Option("input-path,ip", "Encrypted Key file path", "Path to the Encrypted key file", true, null);
        public Option outpath = new Option("output-path,op", "Decrypted Key file path", "Path to the Decrypted key file", true, null);
        public Option pvkPass = new Option("private-pass,pp", "Private Key Password", "Password to decrypt the Private key", true, null);
        public BoolOption OTP = new BoolOption("onetime,ot", "Encrypt result key", "Encrypt the decrypted key with one time passpharse", true, true);
        public Option OTPass = new Option("onetime-pass,s", "One Time Password", "Enter One Time passphrase to Encrypt with", true, null);
        public NoPromptOption prompt = new NoPromptOption("prompt", "Prompt For Options", "Prompt the user for missing options", false, null);
        public NoPromptOption help = new NoPromptOption("help,?", "Usage Help", "Show This Screen", false, null);
        string[] args = null;
        KeyRecovery(string[] cmdargs)
        {
            args = cmdargs;
        }
        static void Main(string[] args)
        {
            string certname = "";
            string pass = "";
            KeyRecovery prg = new KeyRecovery(args);
            prg.OTP.OnOptionEntered = new Option.OptionEnteredHandler(prg.OnOTP);
            prg.ParseArguments();
            pass = prg.pvkPass.Value;
            X509Certificate2 xcert;
           try{
               certname = Path.GetFullPath(prg.keyPath.Value);
                if (pass.Length > 0)
                    xcert = new X509Certificate2(certname, pass);
                else
                    throw new ArgumentNullException("pass");
                prg.rsadec = xcert.PrivateKey as RSACryptoServiceProvider;
                prg.ProcessInputKeyFile();
            }
            catch (Exception exp)
            {
                if(pass == null)
                    Console.WriteLine("Private Key Password cannot be null. Try again. ");
                else
                    Console.WriteLine("Error while processing certificate. Try again");
                    Console.WriteLine(exp.Message);
            }
        }
        void ParseArguments()
        {
            if (args.Length == 0)
            {
                Prompt.CanPrompt = true;
                prompt.Value = true.ToString();
                PromptForArguments();
            }
            else
            {
                Options.ParseArguments(this, args);
                if (help.Assigned)
                {
                    ShowUsage();
                }
                if (prompt.Assigned)
                {
                    Prompt.CanPrompt = true;
                    PromptForArguments();
                }
                else
                {
                    Options.CheckRequiredOptions(this);
                }
            }
        }
        void PromptForArguments()
        {
            Console.WriteLine();
            Option[] options = Options.GetOptions(this);
            foreach (Option option in options)
            {
                Prompt.ForOption(option);
            }
            Console.WriteLine();
            Console.WriteLine("Working...");
            Console.WriteLine();
        }
        private void ShowUsage()
        {
            Console.WriteLine("USAGE: key_converter <Path to Private Key file> <Password for Key file> <Path to Encrypted Key file> <Path to Decrypted key file>");
            Console.WriteLine();
            Console.WriteLine("OPTIONS:");
            Console.WriteLine();
            Option[] options = Options.GetOptions(this);
            foreach (Option o in options)
            {
                int nameCount = 0;
                foreach (string name in o.Names)
                {
                    Console.Write("{0}--{1}", nameCount == 0 ? "\n\t" : ", ", name);
                    nameCount++;
                }
                string description = o.Description == null ? o.Title : o.Description;
                Regex lineSplitter = new Regex(@".{0,50}[^\s]*");
                MatchCollection matches = lineSplitter.Matches(description);
                Console.WriteLine();
                if (o.Required)
                    Console.WriteLine("\t\t(REQUIRED)");
                foreach (Match line in matches)
                {
                    Console.WriteLine("\t\t{0}", line.Value.Trim());
                }
            }
            Console.WriteLine();
            Environment.Exit(-1);
        }
        private bool OnOTP()
        {
            if (OTP.Value == false)
            {
                OTPass.Required = false;
                OTPass.Prompt = false;
            }
            return true;
        }
        private void ProcessInputKeyFile()
        {
            string strKey = string.Format("//{0}/{1}", CollectionIDTag, KeyTag);
            string strID = string.Format("//{0}/{1}", CollectionIDTag, iFolderIDTag);
            string decKey;
            byte[] decKeyByteArray;
            try
            {
                string inKeyPath = Path.GetFullPath(inpath.Value);
                string outKeyPath = Path.GetFullPath(outpath.Value);
                XmlDocument encFile = new XmlDocument();
                encFile.Load(inKeyPath);
                XmlNodeList keyNodeList, idNodeList;
                XmlElement root = encFile.DocumentElement;
                keyNodeList = root.SelectNodes(strKey);
                idNodeList = root.SelectNodes(strID);
                XmlDocument document = new XmlDocument();
                XmlDeclaration xmlDeclaration = document.CreateXmlDeclaration("1.0", "utf-8", null);
                document.InsertBefore(xmlDeclaration, document.DocumentElement);
                XmlElement title = document.CreateElement(titleTag);
                document.AppendChild(title);
                int i = 0;
                foreach (XmlNode idNode in idNodeList)
                {
                    if (idNode.InnerText == null || idNode.InnerText == String.Empty)
                        continue;
                    Console.WriteLine(idNode.InnerText);
                    XmlNode newNode = document.CreateNode("element", CollectionIDTag, "");
                    newNode.InnerText = "";
                    document.DocumentElement.AppendChild(newNode);
                    XmlNode innerNode = document.CreateNode("element", iFolderIDTag, "");
                    innerNode.InnerText = idNode.InnerText;
                    newNode.AppendChild(innerNode);
                    {
                        XmlNode keyNode = keyNodeList[i++];
                        Console.WriteLine(decKey = keyNode.InnerText);
                        decKeyByteArray = Convert.FromBase64String(decKey);
                        XmlNode newElem2 = document.CreateNode("element", KeyTag, "");
                        if (decKey == null || decKey == String.Empty)
                            continue;
                        if (OTP.Value == true)
                            newElem2.InnerText = DecodeMessage(decKeyByteArray, OTPass.Value);
                        else
                            newElem2.InnerText = DecodeMessage(decKeyByteArray);
                        newNode.AppendChild(newElem2);
                    }
                }
                if (File.Exists(outKeyPath))
                    File.Delete(outKeyPath);
                document.Save(outKeyPath);
            }
            catch (Exception e)
            {
                Console.WriteLine("Error while processing input key file. Try again");
                Console.WriteLine(e.Message);
            }
        }
        private string DecodeMessage(byte[] encmess)
        {
            string mess = null;
            try
            {
                mess = Convert.ToBase64String(rsadec.Decrypt(encmess, false));
            }
            catch (Exception cExp)
            {
                Console.WriteLine("Error while decoding message. Try again");
                Console.WriteLine(cExp.Message);
            }
            return mess;
        }
        private string DecodeMessage(byte[] encmess, string otpass)
        {
            string retStr = null;
            byte[] mess;
            try
            {
                mess = rsadec.Decrypt(encmess, false);
                TripleDESCryptoServiceProvider tdesp = new TripleDESCryptoServiceProvider();
                byte[] IV ={ 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 };
                byte[] input = HashPassPhrase(otpass);
                tdesp.KeySize = input.Length * 8;
                byte[] output = Transform(mess, tdesp.CreateEncryptor(input, IV));
                retStr = Convert.ToBase64String(output);
            }
            catch (Exception e)
            {
                Console.WriteLine("Error while decoding message. Try again");
                Console.WriteLine(e.Message);
            }
            return retStr;
        }
        private byte[] HashPassPhrase(string Passphrase)
        {
            byte[] NewPassphrase = null;
            byte[] salt ={ 0x49, 0x46, 0x4F, 0x4C, 0x44, 0x45, 0x52 };
            UTF8Encoding utf8 = new UTF8Encoding();
            byte[] data = utf8.GetBytes(Passphrase);
            try
            {
                HMACSHA1 sha1 = new HMACSHA1();
                sha1.Key = salt;
                for (int i = 0; i < 1000; i++)
                {
                    sha1.ComputeHash(data);
                    data = sha1.Hash;
                }
                NewPassphrase = new byte[data.Length + 4];
                Array.Copy(data, 0, NewPassphrase, 0, data.Length);
                Array.Copy(data, 0, NewPassphrase, 20, 4);
            }
            catch (Exception exp)
            {
                Console.WriteLine("Error while hashing passphrase. Try again");
                Console.WriteLine(exp.Message);
            }
            return NewPassphrase;
        }
        private byte[] Transform(byte[] input, ICryptoTransform CryptoTransform)
        {
            byte[] result = null;
            try
            {
                MemoryStream memStream = new MemoryStream();
                CryptoStream cryptStream = new CryptoStream(memStream, CryptoTransform, CryptoStreamMode.Write);
                cryptStream.Write(input, 0, input.Length);
                cryptStream.FlushFinalBlock();
                memStream.Position = 0;
                result = memStream.ToArray();
                memStream.Close();
                cryptStream.Close();
            }
            catch (Exception exp)
            {
                Console.WriteLine("Exception {0}", exp.Message);
            }
            return result;
        }
    }
}
