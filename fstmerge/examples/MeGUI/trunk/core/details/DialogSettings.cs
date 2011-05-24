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
using System.Text;

using MeGUI.core.util;

namespace MeGUI
{
    [LogByMembers]
    public class DialogSettings
    {
        private bool ovewriteJobOutputResponse = true;

        public bool OverwriteJobOutputResponse
        {
            get { return ovewriteJobOutputResponse; }
            set { ovewriteJobOutputResponse = value; }
        }

        private bool askAboutOverwriteJobOutput = true;

        public bool AskAboutOverwriteJobOutput
        {
            get { return askAboutOverwriteJobOutput; }
            set { askAboutOverwriteJobOutput = value; }
        }


        private bool askAboutDuplicates;
        private bool dupResponse;

        public bool DuplicateResponse
        {
            get { return dupResponse; }
            set { dupResponse = value; }
        }


        public bool AskAboutDuplicates
        {
            get { return askAboutDuplicates; }
            set { askAboutDuplicates = value; }
        }

        private bool askAboutVOBs;
        private bool addConvertToYV12;
        private bool askAboutYV12;
        private bool useOneClick;
        private bool continueDespiteError;
        private bool askAboutError;

        public bool AskAboutError
        {
            get { return askAboutError; }
            set { askAboutError = value; }
        }

        public bool ContinueDespiteError
        {
            get { return continueDespiteError; }
            set { continueDespiteError = value; }
        }


        public bool AskAboutYV12
        {
            get { return askAboutYV12; }
            set { askAboutYV12 = value; }
        }

        public bool AddConvertToYV12
        {
            get { return addConvertToYV12; }
            set { addConvertToYV12 = value; }
        }


        public bool AskAboutVOBs
        {
            get { return askAboutVOBs; }
            set { askAboutVOBs = value; }
        }
        
        public bool UseOneClick
        {
            get { return useOneClick; }
            set { useOneClick = value; }
        }
        public DialogSettings()
        {
            askAboutVOBs = true;
            useOneClick = true;
            askAboutError = true;
            askAboutYV12 = true;
            addConvertToYV12 = true;
            continueDespiteError = true;
            askAboutDuplicates = true;
            dupResponse = true;
        }
    }
}
