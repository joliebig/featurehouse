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

namespace MeGUI
{
	/// <summary>
	/// Summary description for FaacSettings.
	/// </summary>
	public class FaacSettings : AudioCodecSettings
	{
        public static string ID = "FAAC";

        public static readonly int[] SupportedBitrates = new int[] {
            64,
            80,
            96,
            112,
            128,
            160,
            192,
            224,
            256,
            320,
            388,
            448};

		private decimal quality;
		public FaacSettings()
            : base(ID, AudioCodec.AAC, AudioEncoderType.FAAC, 128, BitrateManagementMode.VBR)
		{
			Quality = 100;
		}
		/// <summary>
		/// gets / sets the vbr quality
		/// </summary>
		public decimal Quality
		{
			get {return quality;}
			set {quality = value;}
		}
	}
}
