// ****************************************************************************
// 
// Copyright (C) 2005-2009  Doom9 & al
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// 
// ****************************************************************************

using System;
using System.Collections;
using System.Collections.Generic;

namespace MeGUI
{
	/// <summary>
	/// A container for the selectable languages in MeGUI
	/// </summary>
	public class LanguageSelectionContainer
	{
        private static readonly Dictionary<string,string> languages;
        private static readonly Dictionary<string, string> languagesReverse;

        public static Dictionary<string, string> Languages
        {
            get
            {
                return languages;
            }
        }


        private static void addLanguage(string name, string iso)
        {
            languages.Add(name, iso);
            languagesReverse.Add(iso, name);
        }

        static LanguageSelectionContainer()
        {
            languages = new Dictionary<string, string>();
            languagesReverse = new Dictionary<string, string>();

            addLanguage("English", "eng");
            addLanguage("Abkhazian", "abk");
            addLanguage("Achinese", "ace");
            addLanguage("Acoli", "ach");
            addLanguage("Adangme", "ada");
            addLanguage("Adygei (Adyghe)", "ady");
            addLanguage("Afar", "aar");
            addLanguage("Afrihili", "afh");
            addLanguage("Afrikaans", "afr");
            addLanguage("Afro-Asiatic (Other)", "afa");
            addLanguage("Akan", "aka");
            addLanguage("Akkadian", "akk");
            addLanguage("Albanian", "alb");
            addLanguage("Aleut", "ale");
            addLanguage("Algonquian languages", "alg");
            addLanguage("Altaic (Other)", "tut");
            addLanguage("Apache languages", "apa");
            addLanguage("Arabic", "ara");
            addLanguage("Aragonese", "arg");
            addLanguage("Aramaic", "arc");
            addLanguage("Arapaho", "arp");
            addLanguage("Araucanian", "arn");
            addLanguage("Arawak", "arw");
            addLanguage("Armenian", "arm");
            addLanguage("Assamese", "ast");
            addLanguage("Athapascan languages", "art");
            addLanguage("Australian languages", "aus");
            addLanguage("Austronesian (Other)", "map");
            addLanguage("Avaric", "ava");
            addLanguage("Avestan", "ave");
            addLanguage("Awadhi", "awa");
            addLanguage("Aymara", "aym");
            addLanguage("Azerbaijani", "aze");
            addLanguage("Balinese", "ban");
            addLanguage("Bantu", "bnt");
            addLanguage("Basque", "baq");
            addLanguage("Belarusian", "bel");
            addLanguage("Bosnian", "bos");
            addLanguage("Breton", "bre");
            addLanguage("Bulgarian", "bul");
            addLanguage("Burmese", "bur");
            addLanguage("Catalan", "cat");
            addLanguage("Chinese", "chi");
            addLanguage("Chinese (Alt)", "zho");
            addLanguage("Corsican", "cos");
            addLanguage("Croatian", "scr");
            addLanguage("Czech", "cze");
            addLanguage("Danish", "dan");
            addLanguage("Dutch", "dut");            
            addLanguage("Estonian", "est");
            addLanguage("Faroese", "fao");
            addLanguage("Finnish", "fin");
            addLanguage("Français", "fra");
            addLanguage("French", "fre");
            addLanguage("Georgian", "geo");
            addLanguage("German", "ger");
            addLanguage("Greek", "gre");
            addLanguage("Hebrew", "heb");
            addLanguage("Hindi", "hin");
            addLanguage("Hungarian", "hun");
            addLanguage("Icelandic", "ice");
            addLanguage("Indonesian", "ind");
            addLanguage("Irish", "gai");
            addLanguage("Italian", "ita");
            addLanguage("Japanese", "jpn");
            addLanguage("Kashmiri", "kas");
            addLanguage("Kongo", "kon");
            addLanguage("Korean", "kor");
            addLanguage("Latvian", "lav");
            addLanguage("Lithuanian", "lit");
            addLanguage("Macedonian", "mac");
            addLanguage("Maltese", "mlt");
            addLanguage("Moldavian", "mol");
            addLanguage("Mongolian", "mon");
            addLanguage("Norwegian", "nor");
            addLanguage("Punjabi", "pan");
            addLanguage("Persian", "per");
            addLanguage("Polish", "pol");
            addLanguage("Portuguese", "por");
            addLanguage("Romanian", "ron");
            addLanguage("Russian", "rus");
            addLanguage("Serbian", "scc");
            addLanguage("Slovak", "slk");
            addLanguage("Slovenian", "slv");
            addLanguage("Spanish", "spa");
            addLanguage("Swahili", "swa");
            addLanguage("Swedish", "swe");
            addLanguage("Thai", "tha");
            addLanguage("Tibetan", "tib");
            addLanguage("Turkish", "tur");
            addLanguage("Urdu", "urd");
            addLanguage("Ukrainian", "ukr");
            addLanguage("Uzbek", "uzb");
            addLanguage("Vietnamese", "vie");
            addLanguage("Zhuang", "zha");
            addLanguage("Zulu", "zul");
            addLanguage("Zuni", "zun");
        }

		private LanguageSelectionContainer()
		{
		}

        public static string lookupISOCode(string code)
		{
            if (languagesReverse.ContainsKey(code))
                return languagesReverse[code];
            else
                return "";
		}
        /// <summary>
        /// takes an ISO639.2 3 letter language code and returns
        /// a 2 letter ISO639.1 language code
        /// </summary>
        /// <param name="iso639dot2"></param>
        /// <returns></returns>
        public static string getISO639dot1(string iso639dot2)
        {
            foreach (System.Globalization.CultureInfo ci in System.Globalization.CultureInfo.GetCultures(System.Globalization.CultureTypes.AllCultures))
            {
                if (ci.ThreeLetterISOLanguageName == iso639dot2) // we found our language
                {
                    if (ci.TwoLetterISOLanguageName.Length == 2) // sometimes we get 3 letter codes here, divxmux can't handle those
                        return ci.TwoLetterISOLanguageName;
                }
            }
            return null;
        }

        ///<summary>
        ///Convert the 2 char strings to the full language name
        ///</summary>
        ///
        public static string Short2FullLanguageName(string LngCode)
        {
            string Language = "";
            switch (LngCode.ToLower())
            {
                case "aa": Language = "Afar"; break;
                case "ab": Language = "Abkhazian"; break;
                case "af": Language = "Afrikaans"; break;
                case "am": Language = "Amharic"; break;
                case "ar": Language = "Arabic"; break;
                case "as": Language = "Assamese"; break;
                case "ay": Language = "Aymara"; break;
                case "az": Language = "Azerbaijani"; break;
                case "ba": Language = "Bashkir"; break;
                case "be": Language = "Byelorussian"; break;
                case "bg": Language = "Bulgarian"; break;
                case "bh": Language = "Bihari"; break;
                case "bi": Language = "Bislama"; break;
                case "bn": Language = "Bengali; Bangla"; break;
                case "bo": Language = "Tibetan"; break;
                case "br": Language = "Breton"; break;
                case "ca": Language = "Catalan"; break;
                case "co": Language = "Corsican"; break;
                case "cs": Language = "Czech"; break;
                case "cy": Language = "Welsh"; break;
                case "da": Language = "Danish"; break;
                case "de": Language = "German"; break;
                case "dz": Language = "Bhutani"; break;
                case "el": Language = "Greek"; break;
                case "en": Language = "English"; break;
                case "eo": Language = "Esperanto"; break;
                case "es": Language = "Spanish"; break;
                case "et": Language = "Estonian"; break;
                case "eu": Language = "Basque"; break;
                case "fa": Language = "Persian"; break;
                case "fi": Language = "Finnish"; break;
                case "fj": Language = "Fiji"; break;
                case "fo": Language = "Faroese"; break;
                case "fr": Language = "French"; break;
                case "fy": Language = "Frisian"; break;
                case "ga": Language = "Irish"; break;
                case "gd": Language = "Scots Gaelic"; break;
                case "gl": Language = "Galician"; break;
                case "gn": Language = "Guarani"; break;
                case "gu": Language = "Gujarati"; break;
                case "ha": Language = "Hausa"; break;
                case "he": Language = "Hebrew (formerly iw)"; break;
                case "hi": Language = "Hindi"; break;
                case "hr": Language = "Croatian"; break;
                case "hu": Language = "Hungarian"; break;
                case "hy": Language = "Armenian"; break;
                case "ia": Language = "Interlingua"; break;
                case "id": Language = "Indonesian (formerly in)"; break;
                case "ie": Language = "Interlingue"; break;
                case "ik": Language = "Inupiak"; break;
                case "is": Language = "Icelandic"; break;
                case "it": Language = "Italian"; break;
                case "iu": Language = "Inuktitut"; break;
                case "iw": Language = "Hebrew"; break;
                case "ja": Language = "Japanese"; break;
                case "jw": Language = "Javanese"; break;
                case "ka": Language = "Georgian"; break;
                case "kk": Language = "Kazakh"; break;
                case "kl": Language = "Greenlandic"; break;
                case "km": Language = "Cambodian"; break;
                case "kn": Language = "Kannada"; break;
                case "ko": Language = "Korean"; break;
                case "ks": Language = "Kashmiri"; break;
                case "ku": Language = "Kurdish"; break;
                case "ky": Language = "Kirghiz"; break;
                case "la": Language = "Latin"; break;
                case "ln": Language = "Lingala"; break;
                case "lo": Language = "Laothian"; break;
                case "lt": Language = "Lithuanian"; break;
                case "lv": Language = "Latvian, Lettish"; break;
                case "mg": Language = "Malagasy"; break;
                case "mi": Language = "Maori"; break;
                case "mk": Language = "Macedonian"; break;
                case "ml": Language = "Malayalam"; break;
                case "mn": Language = "Mongolian"; break;
                case "mo": Language = "Moldavian"; break;
                case "mr": Language = "Marathi"; break;
                case "ms": Language = "Malay"; break;
                case "mt": Language = "Maltese"; break;
                case "my": Language = "Burmese"; break;
                case "na": Language = "Nauru"; break;
                case "ne": Language = "Nepali"; break;
                case "nl": Language = "Dutch"; break;
                case "no": Language = "Norwegian"; break;
                case "oc": Language = "Occitan"; break;
                case "om": Language = "(Afan) Oromo"; break;
                case "or": Language = "Oriya"; break;
                case "pa": Language = "Punjabi"; break;
                case "pl": Language = "Polish"; break;
                case "ps": Language = "Pashto, Pushto"; break;
                case "pt": Language = "Portuguese"; break;
                case "qu": Language = "Quechua"; break;
                case "rm": Language = "Rhaeto-Romance"; break;
                case "rn": Language = "Kirundi"; break;
                case "ro": Language = "Romanian"; break;
                case "ru": Language = "Russian"; break;
                case "rw": Language = "Kinyarwanda"; break;
                case "sa": Language = "Sanskrit"; break;
                case "sd": Language = "Sindhi"; break;
                case "sg": Language = "Sangho"; break;
                case "sh": Language = "Serbo-Croatian"; break;
                case "si": Language = "Sinhalese"; break;
                case "sk": Language = "Slovak"; break;
                case "sl": Language = "Slovenian"; break;
                case "sm": Language = "Samoan"; break;
                case "sn": Language = "Shona"; break;
                case "so": Language = "Somali"; break;
                case "sq": Language = "Albanian"; break;
                case "sr": Language = "Serbian"; break;
                case "ss": Language = "Siswati"; break;
                case "st": Language = "Sesotho"; break;
                case "su": Language = "Sundanese"; break;
                case "sv": Language = "Swedish"; break;
                case "sw": Language = "Swahili"; break;
                case "ta": Language = "Tamil"; break;
                case "te": Language = "Telugu"; break;
                case "tg": Language = "Tajik"; break;
                case "th": Language = "Thai"; break;
                case "ti": Language = "Tigrinya"; break;
                case "tk": Language = "Turkmen"; break;
                case "tl": Language = "Tagalog"; break;
                case "tn": Language = "Setswana"; break;
                case "to": Language = "Tonga"; break;
                case "tr": Language = "Turkish"; break;
                case "ts": Language = "Tsonga"; break;
                case "tt": Language = "Tatar"; break;
                case "tw": Language = "Twi"; break;
                case "ug": Language = "Uighur"; break;
                case "uk": Language = "Ukrainian"; break;
                case "ur": Language = "Urdu"; break;
                case "uz": Language = "Uzbek"; break;
                case "vi": Language = "Vietnamese"; break;
                case "vo": Language = "Volapuk"; break;
                case "wo": Language = "Wolof"; break;
                case "xh": Language = "Xhosa"; break;
                case "yi": Language = "Yiddish (formerly ji)"; break;
                case "yo": Language = "Yoruba"; break;
                case "za": Language = "Zhuang"; break;
                case "zh": Language = "Chinese"; break;
                case "zu": Language = "Zulu"; break;
            }
            return Language;
        }
	}
}