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
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace MeGUI.core.util
{
    public class Resolution
    {
        		/// <summary>
		/// if enabled, each change of the horizontal resolution triggers this method
		/// it calculates the ideal mod 16 vertical resolution that matches the desired horizontal resolution
		/// </summary>
		/// <param name="readerHeight">height of the source</param>
		/// <param name="readerWidth">width of the source</param>
		/// <param name="customDAR">custom display aspect ratio to be taken into account for resizing</param>
		/// <param name="cropping">the crop values for the source</param>
		/// <param name="horizontalResolution">the desired horizontal resolution of the output</param>
		/// <param name="signalAR">whether or not we're going to signal the aspect ratio (influences the resizing)</param>
		/// <param name="sarX">horizontal pixel aspect ratio (used when signalAR = true)</param>
		/// <param name="sarY">vertical pixel aspect ratio (used when signalAR = true)</param>
		/// <returns>the suggested horizontal resolution</returns>
		public static int suggestResolution(double readerHeight, double readerWidth, double customDAR, CropValues cropping, int horizontalResolution,
			bool signalAR, int acceptableAspectError, out Dar? dar)
		{
            double fractionOfWidth = (readerWidth - (double)cropping.left - (double)cropping.right) / readerWidth;
            double inputWidthOnHeight = (readerWidth - (double)cropping.left - (double)cropping.right) /
                                          (readerHeight - (double)cropping.top - (double)cropping.bottom);
            double sourceHorizontalResolution = readerHeight * customDAR * fractionOfWidth;
            double sourceVerticalResolution = readerHeight - (double)cropping.top - (double)cropping.bottom;
            double realAspectRatio = sourceHorizontalResolution / sourceVerticalResolution; // the real aspect ratio of the video
            realAspectRatio = getAspectRatio(realAspectRatio, acceptableAspectError); // Constrains DAR to a set of limited possibilities
			double resizedVerticalResolution = (double)horizontalResolution / realAspectRatio;

            int scriptVerticalResolution = ((int)Math.Round(resizedVerticalResolution / 16.0)) * 16;

            if (signalAR)
			{
                resizedVerticalResolution = (double)horizontalResolution / inputWidthOnHeight; // Scale vertical resolution appropriately
                scriptVerticalResolution = ((int)Math.Round(resizedVerticalResolution / 16.0) * 16);

                int parX = 0;
                int parY = 0;
                double distance = 999999;
                for (int i = 1; i < 101; i++)
                {
                    // We create a fraction with integers, and then convert back to a double, and see how big the rounding error is
                    double fractionApproximation = (double)Math.Round(realAspectRatio * ((double)i)) / (double)i;
                    double approximationDifference = Math.Abs(realAspectRatio - fractionApproximation);
                    if (approximationDifference < distance)
                    {
                        distance = approximationDifference;
                        parY = i;
                        parX = (int)Math.Round(realAspectRatio * ((double)parY));
                    }
                }
                Debug.Assert(parX > 0 && parY > 0);
                dar = new Dar((ulong)parX, (ulong)parY);
				
				return scriptVerticalResolution;
			}
			else
			{
                dar = null;
				return scriptVerticalResolution;
			}
		}
		
		/// <summary>
		/// finds the aspect ratio closest to the one giving as parameter (which is an approximation using the selected DAR for the source and the cropping values)
		/// </summary>
		/// <param name="calculatedAR">the aspect ratio to be approximated</param>
		/// <returns>the aspect ratio that most closely matches the input</returns>
		private static double getAspectRatio(double calculatedAR, int acceptableAspectErrorPercent)
		{
			double[] availableAspectRatios = {1.0, 1.33333, 1.66666, 1.77778, 1.85, 2.35};
			double[] distances = new double[availableAspectRatios.Length];
			double minDist = 1000.0;
			double realAspectRatio = 1.0;
			foreach (double d in availableAspectRatios)
			{
                double dist = Math.Abs(d - calculatedAR);
				if (dist < minDist)
				{
					minDist = dist;
					realAspectRatio = d;
				}
			}
            double aspectError = realAspectRatio / calculatedAR;
            if (Math.Abs(aspectError - 1.0) * 100.0 < acceptableAspectErrorPercent)
                return realAspectRatio;
            else
                return calculatedAR;
		}

        /// <summary>
        /// rounds the output PAR to the closest matching predefined xvid profile
        /// </summary>
        /// <param name="sarX">horizontal component of the pixel aspect ratio</param>
        /// <param name="sarY">vertical component of the pixel aspect ratio</param>
        /// <param name="height">height of the desired output</param>
        /// <param name="width">width of the desired output</param>
        /// <returns>the closest profile match</returns>
        public static int roundXviDPAR(int parX, int parY, int height, int width)
        {
            double par = (double) parX / (double) parY;
            double[] pars = { 1, 1.090909, 1.454545, 0.090909, 1.212121 };
            double minDist = 1000;
            int closestIndex = 0;
            for (int i = 0; i < pars.Length; i++)
            {
                double first = Math.Max(par, pars[i]);
                double second = Math.Min(par, pars[i]);
                double dist = first - second;
                if (dist < minDist)
                {
                    minDist = dist;
                    closestIndex = i;
                }
            }
            return closestIndex;
        }
    }
}
