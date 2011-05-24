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
	/// Summary description for snowSettings.
	/// </summary>
	[Serializable]
	public class snowSettings : VideoCodecSettings
	{
        public static readonly string ID = "Snow";
		private decimal quantizer, nbMotionPredictors;
		private int predictionMode, meCompFullpel, meCompHpel, mbComp;
		private bool losslessMode;

        /// <summary>
		/// default contructor, initializes codec default values
		/// </summary>
		public snowSettings():base(ID, VideoEncoderType.SNOW)
		{
			EncodingMode = 0;
			BitrateQuantizer = 700;
			quantizer = new decimal(5);
			predictionMode = 0;
			QPel = false;
			V4MV = false;
			nbMotionPredictors = 0;
			meCompFullpel = 0;
			meCompHpel = 0;
			mbComp = 0;
			losslessMode = false;
			CreditsQuantizer = new decimal(20);
            FourCCs = m_fourCCs;
		}
		

        /// I believe whe really does'nt need to create this array @ per-instance basis
        private static readonly string[] m_fourCCs = { "SNOW" };

		public int PredictionMode
		{
			get {return predictionMode;}
			set {predictionMode = value;}
		}
		public int MECompFullpel
		{
			get {return meCompFullpel;}
			set {meCompFullpel = value;}
		}
		public int MECompHpel
		{
			get {return meCompHpel;}
			set {meCompHpel = value;}
		}
		public int MBComp
		{
			get {return mbComp;}
			set {mbComp = value;}
		}
		public decimal Quantizer
		{
			get {return quantizer;}
			set {quantizer = value;}
		}
		public decimal NbMotionPredictors
		{
			get {return nbMotionPredictors;}
			set {nbMotionPredictors = value;}
		}
		public bool LosslessMode
		{
			get {return losslessMode;}
			set {losslessMode = value;}
		}
		

        public override bool UsesSAR
        {
            get { return false; }
        }

    }
}
