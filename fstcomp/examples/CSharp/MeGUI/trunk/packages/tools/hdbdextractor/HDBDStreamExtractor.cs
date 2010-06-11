// ****************************************************************************
// 
// Copyright (C) 2005-2008  Doom9 & al
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
using System.ComponentModel;
using System.Diagnostics;
using System.Text;
using System.Text.RegularExpressions;
using System.Windows.Forms;

using MeGUI.core.plugins.interfaces;
using MeGUI.core.details;
using eac3to;

namespace MeGUI.packages.tools.hdbdextractor
{
    public class HdBdStreamExtractor : Form
    {
        List<Feature> features;
        List<Stream> streams;
        MainForm info;
        BackgroundWorker backgroundWorker;
        string eac3toPath;
        private MeGUISettings settings;
        private HDStreamsExJob lastJob = null;
        private int inputType = 1;
        string dummyInput = "";
        bool seamless = false;

        #region Windows Form Designer generated code
        private System.Windows.Forms.FolderBrowserDialog folderBrowserDialog1;
        private System.Windows.Forms.GroupBox InputGroupBox;
        private System.Windows.Forms.TextBox FolderInputTextBox;
        private System.Windows.Forms.Button FolderInputSourceButton;
        private System.Windows.Forms.BindingSource FeatureBindingSource;
        private System.Windows.Forms.GroupBox FeatureGroupBox;
        private System.Windows.Forms.BindingSource StreamsBindingSource;
        private System.Windows.Forms.GroupBox StreamGroupBox;
        private MeGUI.packages.tools.hdbdextractor.CustomDataGridView StreamDataGridView;
        private System.Windows.Forms.TextBox LogTextBox;
        private System.Windows.Forms.Button HelpButton2;
        private System.Windows.Forms.LinkLabel Eac3toLinkLabel;
        private System.Windows.Forms.Button QueueButton;
        private System.Windows.Forms.Button CancelButton2;
        private System.Windows.Forms.StatusStrip StatusStrip;
        private System.Windows.Forms.ToolStripStatusLabel ToolStripStatusLabel;
        private System.Windows.Forms.ToolStripProgressBar ToolStripProgressBar;
        private MeGUI.packages.tools.hdbdextractor.CustomDataGridView FeatureDataGridView;
        private DataGridViewTextBoxColumn FeatureNumberDataGridViewTextBoxColumn1;
        private DataGridViewTextBoxColumn FeatureNameDataGridViewTextBoxColumn;
        private DataGridViewTextBoxColumn FeatureDescriptionDataGridViewTextBoxColumn;
        private DataGridViewComboBoxColumn FeatureFileDataGridViewComboBoxColumn;
        private DataGridViewTextBoxColumn FeatureDurationDataGridViewTextBoxColumn;
        private GroupBox OutputGroupBox;
        private Button FolderOutputSourceButton;
        private TextBox FolderOutputTextBox;
        private RadioButton FolderSelection;
        private RadioButton FileSelection;
        private OpenFileDialog openFileDialog1;
        private Button FeatureButton;
        private BindingSource extractTypesBindingSource;
        private DataGridViewCheckBoxColumn StreamExtractCheckBox;
        private DataGridViewTextBoxColumn StreamNumberTextBox;
        private DataGridViewTextBoxColumn StreamTypeTextBox;
        private DataGridViewTextBoxColumn StreamDescriptionTextBox;
        private DataGridViewComboBoxColumn StreamExtractAsComboBox;
        private DataGridViewTextBoxColumn StreamAddOptionsTextBox;
        private DataGridViewTextBoxColumn numberDataGridViewTextBoxColumn;
        private DataGridViewTextBoxColumn nameDataGridViewTextBoxColumn;
        private DataGridViewTextBoxColumn typeDataGridViewTextBoxColumn;
        private DataGridViewTextBoxColumn languageDataGridViewTextBoxColumn;
        private CheckBox closeOnQueue;

        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle1 = new System.Windows.Forms.DataGridViewCellStyle();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(HdBdStreamExtractor));
            this.LogTextBox = new System.Windows.Forms.TextBox();
            this.StatusStrip = new System.Windows.Forms.StatusStrip();
            this.ToolStripStatusLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.ToolStripProgressBar = new System.Windows.Forms.ToolStripProgressBar();
            this.FolderInputTextBox = new System.Windows.Forms.TextBox();
            this.folderBrowserDialog1 = new System.Windows.Forms.FolderBrowserDialog();
            this.HelpButton2 = new System.Windows.Forms.Button();
            this.QueueButton = new System.Windows.Forms.Button();
            this.CancelButton2 = new System.Windows.Forms.Button();
            this.InputGroupBox = new System.Windows.Forms.GroupBox();
            this.FileSelection = new System.Windows.Forms.RadioButton();
            this.FolderSelection = new System.Windows.Forms.RadioButton();
            this.FolderInputSourceButton = new System.Windows.Forms.Button();
            this.Eac3toLinkLabel = new System.Windows.Forms.LinkLabel();
            this.FeatureGroupBox = new System.Windows.Forms.GroupBox();
            this.FeatureDataGridView = new MeGUI.packages.tools.hdbdextractor.CustomDataGridView();
            this.FeatureNumberDataGridViewTextBoxColumn1 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.FeatureNameDataGridViewTextBoxColumn = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.FeatureDescriptionDataGridViewTextBoxColumn = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.FeatureFileDataGridViewComboBoxColumn = new System.Windows.Forms.DataGridViewComboBoxColumn();
            this.FeatureDurationDataGridViewTextBoxColumn = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.FeatureBindingSource = new System.Windows.Forms.BindingSource(this.components);
            this.StreamGroupBox = new System.Windows.Forms.GroupBox();
            this.StreamDataGridView = new MeGUI.packages.tools.hdbdextractor.CustomDataGridView();
            this.StreamExtractCheckBox = new System.Windows.Forms.DataGridViewCheckBoxColumn();
            this.StreamNumberTextBox = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.StreamTypeTextBox = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.StreamDescriptionTextBox = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.StreamExtractAsComboBox = new System.Windows.Forms.DataGridViewComboBoxColumn();
            this.StreamAddOptionsTextBox = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.numberDataGridViewTextBoxColumn = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.nameDataGridViewTextBoxColumn = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.typeDataGridViewTextBoxColumn = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.languageDataGridViewTextBoxColumn = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.StreamsBindingSource = new System.Windows.Forms.BindingSource(this.components);
            this.OutputGroupBox = new System.Windows.Forms.GroupBox();
            this.FolderOutputSourceButton = new System.Windows.Forms.Button();
            this.FolderOutputTextBox = new System.Windows.Forms.TextBox();
            this.openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
            this.FeatureButton = new System.Windows.Forms.Button();
            this.extractTypesBindingSource = new System.Windows.Forms.BindingSource(this.components);
            this.closeOnQueue = new System.Windows.Forms.CheckBox();
            this.StatusStrip.SuspendLayout();
            this.InputGroupBox.SuspendLayout();
            this.FeatureGroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.FeatureDataGridView)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.FeatureBindingSource)).BeginInit();
            this.StreamGroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.StreamDataGridView)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.StreamsBindingSource)).BeginInit();
            this.OutputGroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.extractTypesBindingSource)).BeginInit();
            this.SuspendLayout();
            // 
            // LogTextBox
            // 
            this.LogTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.LogTextBox.Location = new System.Drawing.Point(12, 425);
            this.LogTextBox.Multiline = true;
            this.LogTextBox.Name = "LogTextBox";
            this.LogTextBox.ReadOnly = true;
            this.LogTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.LogTextBox.Size = new System.Drawing.Size(558, 0);
            this.LogTextBox.TabIndex = 7;
            this.LogTextBox.Visible = false;
            // 
            // StatusStrip
            // 
            this.StatusStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.ToolStripStatusLabel,
            this.ToolStripProgressBar});
            this.StatusStrip.Location = new System.Drawing.Point(0, 456);
            this.StatusStrip.Name = "StatusStrip";
            this.StatusStrip.ShowItemToolTips = true;
            this.StatusStrip.Size = new System.Drawing.Size(580, 22);
            this.StatusStrip.TabIndex = 11;
            // 
            // ToolStripStatusLabel
            // 
            this.ToolStripStatusLabel.AutoSize = false;
            this.ToolStripStatusLabel.Name = "ToolStripStatusLabel";
            this.ToolStripStatusLabel.Size = new System.Drawing.Size(358, 17);
            this.ToolStripStatusLabel.Text = "Ready";
            this.ToolStripStatusLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.ToolStripStatusLabel.ToolTipText = "Status";
            // 
            // ToolStripProgressBar
            // 
            this.ToolStripProgressBar.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right;
            this.ToolStripProgressBar.Name = "ToolStripProgressBar";
            this.ToolStripProgressBar.Size = new System.Drawing.Size(200, 16);
            this.ToolStripProgressBar.ToolTipText = "Progress";
            // 
            // FolderInputTextBox
            // 
            this.FolderInputTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.FolderInputTextBox.Location = new System.Drawing.Point(6, 19);
            this.FolderInputTextBox.Name = "FolderInputTextBox";
            this.FolderInputTextBox.Size = new System.Drawing.Size(514, 20);
            this.FolderInputTextBox.TabIndex = 0;
            // 
            // HelpButton2
            // 
            this.HelpButton2.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.HelpButton2.AutoSize = true;
            this.HelpButton2.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.HelpButton2.Location = new System.Drawing.Point(12, 425);
            this.HelpButton2.Name = "HelpButton2";
            this.HelpButton2.Size = new System.Drawing.Size(39, 23);
            this.HelpButton2.TabIndex = 8;
            this.HelpButton2.Text = "Help";
            this.HelpButton2.UseVisualStyleBackColor = true;
            this.HelpButton2.Click += new System.EventHandler(this.HelpButton2_Click);
            // 
            // QueueButton
            // 
            this.QueueButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.QueueButton.Location = new System.Drawing.Point(414, 425);
            this.QueueButton.Name = "QueueButton";
            this.QueueButton.Size = new System.Drawing.Size(75, 23);
            this.QueueButton.TabIndex = 9;
            this.QueueButton.Text = "Queue";
            this.QueueButton.UseVisualStyleBackColor = true;
            this.QueueButton.Click += new System.EventHandler(this.QueueButton_Click);
            // 
            // CancelButton2
            // 
            this.CancelButton2.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.CancelButton2.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.CancelButton2.Location = new System.Drawing.Point(495, 425);
            this.CancelButton2.Name = "CancelButton2";
            this.CancelButton2.Size = new System.Drawing.Size(75, 23);
            this.CancelButton2.TabIndex = 10;
            this.CancelButton2.Text = "Cancel";
            this.CancelButton2.UseVisualStyleBackColor = true;
            this.CancelButton2.Click += new System.EventHandler(this.CancelButton2_Click);
            // 
            // InputGroupBox
            // 
            this.InputGroupBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.InputGroupBox.Controls.Add(this.FileSelection);
            this.InputGroupBox.Controls.Add(this.FolderSelection);
            this.InputGroupBox.Controls.Add(this.FolderInputSourceButton);
            this.InputGroupBox.Controls.Add(this.FolderInputTextBox);
            this.InputGroupBox.Location = new System.Drawing.Point(12, 3);
            this.InputGroupBox.Name = "InputGroupBox";
            this.InputGroupBox.Size = new System.Drawing.Size(558, 74);
            this.InputGroupBox.TabIndex = 0;
            this.InputGroupBox.TabStop = false;
            this.InputGroupBox.Text = "Input";
            // 
            // FileSelection
            // 
            this.FileSelection.AutoSize = true;
            this.FileSelection.Location = new System.Drawing.Point(170, 46);
            this.FileSelection.Name = "FileSelection";
            this.FileSelection.Size = new System.Drawing.Size(115, 17);
            this.FileSelection.TabIndex = 14;
            this.FileSelection.TabStop = true;
            this.FileSelection.Text = "Select File as Input";
            this.FileSelection.UseVisualStyleBackColor = true;
            // 
            // FolderSelection
            // 
            this.FolderSelection.AutoSize = true;
            this.FolderSelection.Checked = true;
            this.FolderSelection.Location = new System.Drawing.Point(18, 46);
            this.FolderSelection.Name = "FolderSelection";
            this.FolderSelection.Size = new System.Drawing.Size(128, 17);
            this.FolderSelection.TabIndex = 13;
            this.FolderSelection.TabStop = true;
            this.FolderSelection.Text = "Select Folder as Input";
            this.FolderSelection.UseVisualStyleBackColor = true;
            // 
            // FolderInputSourceButton
            // 
            this.FolderInputSourceButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.FolderInputSourceButton.AutoSize = true;
            this.FolderInputSourceButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.FolderInputSourceButton.Location = new System.Drawing.Point(526, 16);
            this.FolderInputSourceButton.Name = "FolderInputSourceButton";
            this.FolderInputSourceButton.Size = new System.Drawing.Size(26, 23);
            this.FolderInputSourceButton.TabIndex = 12;
            this.FolderInputSourceButton.Text = "...";
            this.FolderInputSourceButton.UseVisualStyleBackColor = true;
            this.FolderInputSourceButton.Click += new System.EventHandler(this.FolderInputSourceButton_Click);
            // 
            // Eac3toLinkLabel
            // 
            this.Eac3toLinkLabel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.Eac3toLinkLabel.AutoSize = true;
            this.Eac3toLinkLabel.Location = new System.Drawing.Point(57, 430);
            this.Eac3toLinkLabel.Name = "Eac3toLinkLabel";
            this.Eac3toLinkLabel.Size = new System.Drawing.Size(40, 13);
            this.Eac3toLinkLabel.TabIndex = 13;
            this.Eac3toLinkLabel.TabStop = true;
            this.Eac3toLinkLabel.Text = "eac3to";
            this.Eac3toLinkLabel.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.Eac3toLinkLabel_LinkClicked);
            // 
            // FeatureGroupBox
            // 
            this.FeatureGroupBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.FeatureGroupBox.Controls.Add(this.FeatureDataGridView);
            this.FeatureGroupBox.Location = new System.Drawing.Point(12, 134);
            this.FeatureGroupBox.Name = "FeatureGroupBox";
            this.FeatureGroupBox.Size = new System.Drawing.Size(558, 110);
            this.FeatureGroupBox.TabIndex = 14;
            this.FeatureGroupBox.TabStop = false;
            this.FeatureGroupBox.Text = "Feature(s)";
            // 
            // FeatureDataGridView
            // 
            this.FeatureDataGridView.AllowUserToAddRows = false;
            this.FeatureDataGridView.AllowUserToDeleteRows = false;
            this.FeatureDataGridView.AllowUserToResizeColumns = false;
            this.FeatureDataGridView.AllowUserToResizeRows = false;
            this.FeatureDataGridView.AutoGenerateColumns = false;
            this.FeatureDataGridView.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells;
            this.FeatureDataGridView.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.FeatureDataGridView.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.FeatureNumberDataGridViewTextBoxColumn1,
            this.FeatureNameDataGridViewTextBoxColumn,
            this.FeatureDescriptionDataGridViewTextBoxColumn,
            this.FeatureFileDataGridViewComboBoxColumn,
            this.FeatureDurationDataGridViewTextBoxColumn});
            this.FeatureDataGridView.DataSource = this.FeatureBindingSource;
            this.FeatureDataGridView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.FeatureDataGridView.Location = new System.Drawing.Point(3, 16);
            this.FeatureDataGridView.MultiSelect = false;
            this.FeatureDataGridView.Name = "FeatureDataGridView";
            this.FeatureDataGridView.RowHeadersVisible = false;
            this.FeatureDataGridView.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.FeatureDataGridView.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect;
            this.FeatureDataGridView.ShowEditingIcon = false;
            this.FeatureDataGridView.Size = new System.Drawing.Size(552, 91);
            this.FeatureDataGridView.TabIndex = 13;
            this.FeatureDataGridView.RowLeave += new System.Windows.Forms.DataGridViewCellEventHandler(this.FeatureDataGridView_RowLeave);
            this.FeatureDataGridView.DataBindingComplete += new System.Windows.Forms.DataGridViewBindingCompleteEventHandler(this.FeatureDataGridView_DataBindingComplete);
            this.FeatureDataGridView.DataSourceChanged += new System.EventHandler(this.FeatureDataGridView_DataSourceChanged);
            // 
            // FeatureNumberDataGridViewTextBoxColumn1
            // 
            this.FeatureNumberDataGridViewTextBoxColumn1.DataPropertyName = "Number";
            this.FeatureNumberDataGridViewTextBoxColumn1.HeaderText = "#";
            this.FeatureNumberDataGridViewTextBoxColumn1.MinimumWidth = 26;
            this.FeatureNumberDataGridViewTextBoxColumn1.Name = "FeatureNumberDataGridViewTextBoxColumn1";
            this.FeatureNumberDataGridViewTextBoxColumn1.ReadOnly = true;
            this.FeatureNumberDataGridViewTextBoxColumn1.Resizable = System.Windows.Forms.DataGridViewTriState.False;
            this.FeatureNumberDataGridViewTextBoxColumn1.ToolTipText = "Feature number";
            this.FeatureNumberDataGridViewTextBoxColumn1.Width = 26;
            // 
            // FeatureNameDataGridViewTextBoxColumn
            // 
            this.FeatureNameDataGridViewTextBoxColumn.DataPropertyName = "Name";
            this.FeatureNameDataGridViewTextBoxColumn.HeaderText = "Name";
            this.FeatureNameDataGridViewTextBoxColumn.MinimumWidth = 125;
            this.FeatureNameDataGridViewTextBoxColumn.Name = "FeatureNameDataGridViewTextBoxColumn";
            this.FeatureNameDataGridViewTextBoxColumn.ReadOnly = true;
            this.FeatureNameDataGridViewTextBoxColumn.Resizable = System.Windows.Forms.DataGridViewTriState.False;
            this.FeatureNameDataGridViewTextBoxColumn.ToolTipText = "Feature name";
            this.FeatureNameDataGridViewTextBoxColumn.Width = 125;
            // 
            // FeatureDescriptionDataGridViewTextBoxColumn
            // 
            this.FeatureDescriptionDataGridViewTextBoxColumn.DataPropertyName = "Description";
            this.FeatureDescriptionDataGridViewTextBoxColumn.HeaderText = "Description";
            this.FeatureDescriptionDataGridViewTextBoxColumn.MinimumWidth = 244;
            this.FeatureDescriptionDataGridViewTextBoxColumn.Name = "FeatureDescriptionDataGridViewTextBoxColumn";
            this.FeatureDescriptionDataGridViewTextBoxColumn.ReadOnly = true;
            this.FeatureDescriptionDataGridViewTextBoxColumn.Resizable = System.Windows.Forms.DataGridViewTriState.False;
            this.FeatureDescriptionDataGridViewTextBoxColumn.ToolTipText = "Feature description";
            this.FeatureDescriptionDataGridViewTextBoxColumn.Width = 244;
            // 
            // FeatureFileDataGridViewComboBoxColumn
            // 
            this.FeatureFileDataGridViewComboBoxColumn.HeaderText = "File(s)";
            this.FeatureFileDataGridViewComboBoxColumn.MinimumWidth = 90;
            this.FeatureFileDataGridViewComboBoxColumn.Name = "FeatureFileDataGridViewComboBoxColumn";
            this.FeatureFileDataGridViewComboBoxColumn.Resizable = System.Windows.Forms.DataGridViewTriState.False;
            this.FeatureFileDataGridViewComboBoxColumn.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Automatic;
            this.FeatureFileDataGridViewComboBoxColumn.ToolTipText = "Feature File(s)";
            this.FeatureFileDataGridViewComboBoxColumn.Width = 90;
            // 
            // FeatureDurationDataGridViewTextBoxColumn
            // 
            this.FeatureDurationDataGridViewTextBoxColumn.DataPropertyName = "Duration";
            this.FeatureDurationDataGridViewTextBoxColumn.HeaderText = "Duration";
            this.FeatureDurationDataGridViewTextBoxColumn.MinimumWidth = 52;
            this.FeatureDurationDataGridViewTextBoxColumn.Name = "FeatureDurationDataGridViewTextBoxColumn";
            this.FeatureDurationDataGridViewTextBoxColumn.ReadOnly = true;
            this.FeatureDurationDataGridViewTextBoxColumn.Resizable = System.Windows.Forms.DataGridViewTriState.False;
            this.FeatureDurationDataGridViewTextBoxColumn.Width = 52;
            // 
            // FeatureBindingSource
            // 
            this.FeatureBindingSource.AllowNew = false;
            this.FeatureBindingSource.DataSource = typeof(eac3to.Feature);
            // 
            // StreamGroupBox
            // 
            this.StreamGroupBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.StreamGroupBox.Controls.Add(this.StreamDataGridView);
            this.StreamGroupBox.Location = new System.Drawing.Point(12, 250);
            this.StreamGroupBox.Name = "StreamGroupBox";
            this.StreamGroupBox.Size = new System.Drawing.Size(558, 169);
            this.StreamGroupBox.TabIndex = 15;
            this.StreamGroupBox.TabStop = false;
            this.StreamGroupBox.Text = "Stream(s)";
            // 
            // StreamDataGridView
            // 
            this.StreamDataGridView.AllowUserToAddRows = false;
            this.StreamDataGridView.AllowUserToDeleteRows = false;
            this.StreamDataGridView.AllowUserToResizeColumns = false;
            this.StreamDataGridView.AllowUserToResizeRows = false;
            dataGridViewCellStyle1.BackColor = System.Drawing.SystemColors.Window;
            this.StreamDataGridView.AlternatingRowsDefaultCellStyle = dataGridViewCellStyle1;
            this.StreamDataGridView.AutoGenerateColumns = false;
            this.StreamDataGridView.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells;
            this.StreamDataGridView.ColumnHeadersHeight = 21;
            this.StreamDataGridView.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.DisableResizing;
            this.StreamDataGridView.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.StreamExtractCheckBox,
            this.StreamNumberTextBox,
            this.StreamTypeTextBox,
            this.StreamDescriptionTextBox,
            this.StreamExtractAsComboBox,
            this.StreamAddOptionsTextBox,
            this.numberDataGridViewTextBoxColumn,
            this.nameDataGridViewTextBoxColumn,
            this.typeDataGridViewTextBoxColumn,
            this.languageDataGridViewTextBoxColumn});
            this.StreamDataGridView.DataSource = this.StreamsBindingSource;
            this.StreamDataGridView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.StreamDataGridView.Location = new System.Drawing.Point(3, 16);
            this.StreamDataGridView.MultiSelect = false;
            this.StreamDataGridView.Name = "StreamDataGridView";
            this.StreamDataGridView.RowHeadersVisible = false;
            this.StreamDataGridView.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.StreamDataGridView.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect;
            this.StreamDataGridView.ShowEditingIcon = false;
            this.StreamDataGridView.Size = new System.Drawing.Size(552, 150);
            this.StreamDataGridView.TabIndex = 7;
            this.StreamDataGridView.DataSourceChanged += new System.EventHandler(this.StreamDataGridView_DataSourceChanged);
            // 
            // StreamExtractCheckBox
            // 
            this.StreamExtractCheckBox.FalseValue = "0";
            this.StreamExtractCheckBox.HeaderText = "Extract?";
            this.StreamExtractCheckBox.IndeterminateValue = "-1";
            this.StreamExtractCheckBox.MinimumWidth = 50;
            this.StreamExtractCheckBox.Name = "StreamExtractCheckBox";
            this.StreamExtractCheckBox.Resizable = System.Windows.Forms.DataGridViewTriState.False;
            this.StreamExtractCheckBox.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Automatic;
            this.StreamExtractCheckBox.ToolTipText = "Extract stream?";
            this.StreamExtractCheckBox.TrueValue = "1";
            this.StreamExtractCheckBox.Width = 50;
            // 
            // StreamNumberTextBox
            // 
            this.StreamNumberTextBox.DataPropertyName = "Number";
            this.StreamNumberTextBox.HeaderText = "#";
            this.StreamNumberTextBox.MinimumWidth = 26;
            this.StreamNumberTextBox.Name = "StreamNumberTextBox";
            this.StreamNumberTextBox.Resizable = System.Windows.Forms.DataGridViewTriState.False;
            this.StreamNumberTextBox.ToolTipText = "Stream Number";
            this.StreamNumberTextBox.Width = 26;
            // 
            // StreamTypeTextBox
            // 
            this.StreamTypeTextBox.DataPropertyName = "Type";
            this.StreamTypeTextBox.HeaderText = "Type";
            this.StreamTypeTextBox.MinimumWidth = 45;
            this.StreamTypeTextBox.Name = "StreamTypeTextBox";
            this.StreamTypeTextBox.Resizable = System.Windows.Forms.DataGridViewTriState.False;
            this.StreamTypeTextBox.ToolTipText = "Stream type";
            this.StreamTypeTextBox.Width = 45;
            // 
            // StreamDescriptionTextBox
            // 
            this.StreamDescriptionTextBox.DataPropertyName = "Description";
            this.StreamDescriptionTextBox.HeaderText = "Description";
            this.StreamDescriptionTextBox.MinimumWidth = 260;
            this.StreamDescriptionTextBox.Name = "StreamDescriptionTextBox";
            this.StreamDescriptionTextBox.Resizable = System.Windows.Forms.DataGridViewTriState.False;
            this.StreamDescriptionTextBox.ToolTipText = "Stream description";
            this.StreamDescriptionTextBox.Width = 260;
            // 
            // StreamExtractAsComboBox
            // 
            this.StreamExtractAsComboBox.HeaderText = "Extract As";
            this.StreamExtractAsComboBox.MinimumWidth = 69;
            this.StreamExtractAsComboBox.Name = "StreamExtractAsComboBox";
            this.StreamExtractAsComboBox.Resizable = System.Windows.Forms.DataGridViewTriState.False;
            this.StreamExtractAsComboBox.ToolTipText = "Stream extract type";
            this.StreamExtractAsComboBox.Width = 69;
            // 
            // StreamAddOptionsTextBox
            // 
            this.StreamAddOptionsTextBox.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.StreamAddOptionsTextBox.HeaderText = "+ Options";
            this.StreamAddOptionsTextBox.MinimumWidth = 65;
            this.StreamAddOptionsTextBox.Name = "StreamAddOptionsTextBox";
            this.StreamAddOptionsTextBox.Resizable = System.Windows.Forms.DataGridViewTriState.False;
            this.StreamAddOptionsTextBox.ToolTipText = "Stream extract additional options";
            // 
            // numberDataGridViewTextBoxColumn
            // 
            this.numberDataGridViewTextBoxColumn.DataPropertyName = "Number";
            this.numberDataGridViewTextBoxColumn.HeaderText = "Number";
            this.numberDataGridViewTextBoxColumn.Name = "numberDataGridViewTextBoxColumn";
            // 
            // nameDataGridViewTextBoxColumn
            // 
            this.nameDataGridViewTextBoxColumn.DataPropertyName = "Name";
            this.nameDataGridViewTextBoxColumn.HeaderText = "Name";
            this.nameDataGridViewTextBoxColumn.Name = "nameDataGridViewTextBoxColumn";
            // 
            // typeDataGridViewTextBoxColumn
            // 
            this.typeDataGridViewTextBoxColumn.DataPropertyName = "Type";
            this.typeDataGridViewTextBoxColumn.HeaderText = "Type";
            this.typeDataGridViewTextBoxColumn.Name = "typeDataGridViewTextBoxColumn";
            // 
            // languageDataGridViewTextBoxColumn
            // 
            this.languageDataGridViewTextBoxColumn.DataPropertyName = "Language";
            this.languageDataGridViewTextBoxColumn.HeaderText = "Language";
            this.languageDataGridViewTextBoxColumn.Name = "languageDataGridViewTextBoxColumn";
            // 
            // StreamsBindingSource
            // 
            this.StreamsBindingSource.DataSource = typeof(eac3to.Stream);
            // 
            // OutputGroupBox
            // 
            this.OutputGroupBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.OutputGroupBox.Controls.Add(this.FolderOutputSourceButton);
            this.OutputGroupBox.Controls.Add(this.FolderOutputTextBox);
            this.OutputGroupBox.Location = new System.Drawing.Point(10, 83);
            this.OutputGroupBox.Name = "OutputGroupBox";
            this.OutputGroupBox.Size = new System.Drawing.Size(558, 45);
            this.OutputGroupBox.TabIndex = 16;
            this.OutputGroupBox.TabStop = false;
            this.OutputGroupBox.Text = "Output";
            // 
            // FolderOutputSourceButton
            // 
            this.FolderOutputSourceButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.FolderOutputSourceButton.AutoSize = true;
            this.FolderOutputSourceButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.FolderOutputSourceButton.Location = new System.Drawing.Point(526, 17);
            this.FolderOutputSourceButton.Name = "FolderOutputSourceButton";
            this.FolderOutputSourceButton.Size = new System.Drawing.Size(26, 23);
            this.FolderOutputSourceButton.TabIndex = 13;
            this.FolderOutputSourceButton.Text = "...";
            this.FolderOutputSourceButton.UseVisualStyleBackColor = true;
            this.FolderOutputSourceButton.Click += new System.EventHandler(this.FolderOutputSourceButton_Click);
            // 
            // FolderOutputTextBox
            // 
            this.FolderOutputTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.FolderOutputTextBox.Location = new System.Drawing.Point(6, 19);
            this.FolderOutputTextBox.Name = "FolderOutputTextBox";
            this.FolderOutputTextBox.Size = new System.Drawing.Size(514, 20);
            this.FolderOutputTextBox.TabIndex = 1;
            // 
            // openFileDialog1
            // 
            this.openFileDialog1.Filter = "E-VOB Files (.*evo,*.vob)|*.evo;*.vob|Transport Streams Files (*.m2t*,*.mts,*.ts)" +
                "|*.m2t*;*.ts|Matroska Files (.*mkv)|*.mkv|All Files supported (*.*)|*.evo;*.vob;*.m2t*;*.mts;*.ts;*.mkv";
            this.openFileDialog1.FilterIndex = 4;
            this.openFileDialog1.Multiselect = true;
            // 
            // FeatureButton
            // 
            this.FeatureButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.FeatureButton.Location = new System.Drawing.Point(333, 425);
            this.FeatureButton.Name = "FeatureButton";
            this.FeatureButton.Size = new System.Drawing.Size(75, 23);
            this.FeatureButton.TabIndex = 18;
            this.FeatureButton.Text = "Features";
            this.FeatureButton.UseVisualStyleBackColor = true;
            this.FeatureButton.Visible = false;
            // 
            // extractTypesBindingSource
            // 
            this.extractTypesBindingSource.DataMember = "ExtractTypes";
            this.extractTypesBindingSource.DataSource = this.StreamsBindingSource;
            // 
            // closeOnQueue
            // 
            this.closeOnQueue.Checked = true;
            this.closeOnQueue.CheckState = System.Windows.Forms.CheckState.Checked;
            this.closeOnQueue.Location = new System.Drawing.Point(336, 425);
            this.closeOnQueue.Name = "closeOnQueue";
            this.closeOnQueue.Size = new System.Drawing.Size(72, 24);
            this.closeOnQueue.TabIndex = 19;
            this.closeOnQueue.Text = "and close";
            // 
            // HdBdStreamExtractor
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(580, 478);
            this.Controls.Add(this.closeOnQueue);
            this.Controls.Add(this.FeatureButton);
            this.Controls.Add(this.OutputGroupBox);
            this.Controls.Add(this.StreamGroupBox);
            this.Controls.Add(this.FeatureGroupBox);
            this.Controls.Add(this.Eac3toLinkLabel);
            this.Controls.Add(this.InputGroupBox);
            this.Controls.Add(this.CancelButton2);
            this.Controls.Add(this.QueueButton);
            this.Controls.Add(this.HelpButton2);
            this.Controls.Add(this.StatusStrip);
            this.Controls.Add(this.LogTextBox);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.Name = "HdBdStreamExtractor";
            this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Hide;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "MeGUI - HD-DVD/Blu-ray Streams Extractor";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.HdBrStreamExtractor_FormClosing);
            this.StatusStrip.ResumeLayout(false);
            this.StatusStrip.PerformLayout();
            this.InputGroupBox.ResumeLayout(false);
            this.InputGroupBox.PerformLayout();
            this.FeatureGroupBox.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.FeatureDataGridView)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.FeatureBindingSource)).EndInit();
            this.StreamGroupBox.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.StreamDataGridView)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.StreamsBindingSource)).EndInit();
            this.OutputGroupBox.ResumeLayout(false);
            this.OutputGroupBox.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.extractTypesBindingSource)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }
        #endregion

        public HdBdStreamExtractor(MainForm info)
        {
            this.info = info;
            this.settings = info.Settings;
            InitializeComponent();

            eac3toPath = settings.EAC3toPath;
        }

        struct eac3toArgs
        {
            public string eac3toPath { get; set; }
            public string inputPath { get; set; }
            public string workingFolder { get; set; }
            public string featureNumber { get; set; }
            public string args { get; set; }
            public ResultState resultState { get; set; }

            public eac3toArgs(string eac3toPath, string inputPath, string args)
                : this()
            {
                this.eac3toPath = eac3toPath;
                this.inputPath = inputPath;
                this.args = args;
            }
        }

        public enum ResultState
        {
            [StringValue("Feature Retrieval Completed")]
            FeatureCompleted,
            [StringValue("Stream Retrieval Completed")]
            StreamCompleted,
            [StringValue("Stream Extraction Completed")]
            ExtractCompleted
        }

        #region backgroundWorker
        void backgroundWorker_DoWork(object sender, DoWorkEventArgs e)
        {
            eac3toArgs args = (eac3toArgs)e.Argument;

            using (Process compiler = new Process())
            {
                compiler.StartInfo.FileName = args.eac3toPath;
                // use tester to debug posted logs from forums
                //compiler.StartInfo.FileName = "tester.exe";

                switch (args.resultState)
                {
                    case ResultState.FeatureCompleted:
                        compiler.StartInfo.Arguments = string.Format("\"{0}\"", args.inputPath);
                        //use commented line below for debuging posted feature logs from forums
                        //compiler.StartInfo.Arguments = "\"Tests\\featuers\\New Text Document.txt\"";
                        break;
                    case ResultState.StreamCompleted:
                        if (args.args == string.Empty)
                             compiler.StartInfo.Arguments = string.Format("\"{0}\"", args.inputPath);
                        else compiler.StartInfo.Arguments = string.Format("\"{0}\" {1}) {2}", args.inputPath, args.args, "-progressnumbers");
                        //use commented line below for debuging posted stream logs from forums
                        //compiler.StartInfo.Arguments = "\"Tests\\streams\\New Text Document (4).txt\"";
                        break;
                    case ResultState.ExtractCompleted:
                        if (FileSelection.Checked)
                             compiler.StartInfo.Arguments = string.Format("\"{0}\" {1}", args.inputPath, args.args + " -progressnumbers");
                        else compiler.StartInfo.Arguments = string.Format("\"{0}\" {1}) {2}", args.inputPath, args.featureNumber, args.args + "-progressnumbers");
                        break;
                }

                WriteToLog(string.Format("Arguments: {0}", compiler.StartInfo.Arguments));

                compiler.StartInfo.WorkingDirectory = args.workingFolder;
                compiler.StartInfo.CreateNoWindow = true;
                compiler.StartInfo.UseShellExecute = false;
                compiler.StartInfo.RedirectStandardOutput = true;
                compiler.StartInfo.RedirectStandardError = true;
                compiler.StartInfo.ErrorDialog = false;
                compiler.EnableRaisingEvents = true;

                compiler.EnableRaisingEvents = true;
                compiler.Exited += new EventHandler(backgroundWorker_Exited);
                compiler.ErrorDataReceived += new DataReceivedEventHandler(backgroundWorker_ErrorDataReceived);
                compiler.OutputDataReceived += new DataReceivedEventHandler(backgroundWorker_OutputDataReceived);

                try
                {
                    compiler.Start();
                    compiler.BeginErrorReadLine();
                    compiler.BeginOutputReadLine();

                    while (!compiler.HasExited)
                        if (backgroundWorker.CancellationPending)
                            compiler.Kill();

                    compiler.WaitForExit();
                }
                catch (Exception ex)
                {
                    //e.Cancel = true;
                    //e.Result = ex.Message;
                    WriteToLog(ex.Message);
                }
                finally
                {
                    compiler.ErrorDataReceived -= new DataReceivedEventHandler(backgroundWorker_ErrorDataReceived);
                    compiler.OutputDataReceived -= new DataReceivedEventHandler(backgroundWorker_OutputDataReceived);
                }
            }

            e.Result = args.resultState;
        }

        void backgroundWorker_ProgressChanged(object sender, ProgressChangedEventArgs e)
        {
            SetToolStripProgressBarValue(e.ProgressPercentage);

            if (e.UserState != null)
                SetToolStripLabelText(e.UserState.ToString());
        }

        void backgroundWorker_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            if (e.Cancelled)
                WriteToLog("Work was cancelled");

            if (e.Error != null)
            {
                WriteToLog(e.Error.Message);
                WriteToLog(e.Error.TargetSite.Name);
                WriteToLog(e.Error.Source);
                WriteToLog(e.Error.StackTrace);
            }

            SetToolStripProgressBarValue(0);
            SetToolStripLabelText(Extensions.GetStringValue(((ResultState)e.Result)));

            if (e.Result != null)
            {
                WriteToLog(Extensions.GetStringValue(((ResultState)e.Result)));

                switch ((ResultState)e.Result)
                {
                    case ResultState.FeatureCompleted:
                        FeatureDataGridView.DataSource = features;
                        FeatureButton.Enabled = true;
                        FeatureDataGridView.SelectionChanged += new System.EventHandler(this.FeatureDataGridView_SelectionChanged);
                        break;
                    case ResultState.StreamCompleted:
                        if (FileSelection.Checked)
                             StreamDataGridView.DataSource = streams;
                        else StreamDataGridView.DataSource = ((eac3to.Feature)FeatureDataGridView.SelectedRows[0].DataBoundItem).Streams;
                        FeatureButton.Enabled = true;
                        break;
                    case ResultState.ExtractCompleted:
                        QueueButton.Enabled = true;
                        break;
                }
            }
        }

        void backgroundWorker_Exited(object sender, EventArgs e)
        {
            ResetCursor(Cursors.Default);
        }

        void backgroundWorker_ErrorDataReceived(object sender, DataReceivedEventArgs e)
        {
            string data;

            if (!String.IsNullOrEmpty(e.Data))
            {
                data = e.Data.TrimStart('\b').Trim();

                if (!string.IsNullOrEmpty(data))
                    WriteToLog("Error: " + e.Data);
            }
        }

        void backgroundWorker_OutputDataReceived(object sender, DataReceivedEventArgs e)
        {
            string data;

            if (!string.IsNullOrEmpty(e.Data))
            {
                data = e.Data.TrimStart('\b').Trim();

                if (!string.IsNullOrEmpty(data))
                {
                    // Feature line
                    // 2) 00216.mpls, 0:50:19
                    if (Regex.IsMatch(data, @"^[0-99]+\).+$", RegexOptions.Compiled))
                    {
                        try
                        {
                            features.Add(eac3to.Feature.Parse(data));
                        }
                        catch (Exception ex)
                        {
                            WriteToLog(ex.Message);
                            WriteToLog(ex.Source);
                            WriteToLog(ex.StackTrace);
                        }

                        return;
                    }

                    // Feature name
                    // "Feature Name"
                    else if (Regex.IsMatch(data, "^\".+\"$", RegexOptions.Compiled))
                    {
                        if (FileSelection.Checked)
                             streams[streams.Count - 1].Name = Extensions.CapitalizeAll(data.Trim("\" .".ToCharArray()));
                        else features[features.Count - 1].Name = Extensions.CapitalizeAll(data.Trim("\" .".ToCharArray()));
                        return;
                    }

                    // Stream line on feature listing
                    // - h264/AVC, 1080p24 /1.001 (16:9)
                    else if (Regex.IsMatch(data, "^-.+$", RegexOptions.Compiled))
                        return;

                    // Playlist file listing
                    // [99+100+101+102+103+104+105+106+114].m2ts (blueray playlist *.mpls)
                    else if (Regex.IsMatch(data, @"^\[.+\].m2ts$", RegexOptions.Compiled))
                    {
                        foreach (string file in Regex.Match(data, @"\[.+\]").Value.Trim("[]".ToCharArray()).Split("+".ToCharArray()))
                            features[features.Count - 1].Files.Add(new File(file + ".m2ts", features[features.Count - 1].Files.Count + 1));

                        return;
                    }

                    // Stream listing feature header
                    // M2TS, 1 video track, 6 audio tracks, 9 subtitle tracks, 1:53:06
                    // EVO, 2 video tracks, 4 audio tracks, 8 subtitle tracks, 2:20:02
                    else if (Regex.IsMatch(data, "^M2TS, .+$", RegexOptions.Compiled) || 
                             Regex.IsMatch(data, "^EVO, .+$", RegexOptions.Compiled)  ||
                             Regex.IsMatch(data, "^TS, .+$", RegexOptions.Compiled)   ||
                             Regex.IsMatch(data, "^VOB, .+$", RegexOptions.Compiled)  ||
                             Regex.IsMatch(data, "^MKV, .+$", RegexOptions.Compiled)  ||
                             Regex.IsMatch(data, "^MKA, .+$", RegexOptions.Compiled)   
                             )
                    {
                        WriteToLog(data);
                        return;
                    }

                    // Stream line
                    // 8: AC3, English, 2.0 channels, 192kbps, 48khz, dialnorm: -27dB
                    else if (Regex.IsMatch(data, "^[0-99]+:.+$", RegexOptions.Compiled))
                    {
                        if (FileSelection.Checked)
                        {
                            try
                            {
                                streams.Add(eac3to.Stream.Parse(data));
                            }
                            catch (Exception ex)
                            {
                                WriteToLog(ex.Message);
                                WriteToLog(ex.Source);
                                WriteToLog(ex.StackTrace);
                            }
                        }
                        else
                        {
                            try
                            {
                                if (FeatureDataGridView.SelectedRows.Count == 0)
                                    FeatureDataGridView.Rows[0].Selected = true;
                                ((Feature)FeatureDataGridView.SelectedRows[0].DataBoundItem).Streams.Add(Stream.Parse(data));
                            }
                            catch (Exception ex)
                            {
                                WriteToLog(ex.Message);
                                WriteToLog(ex.Source);
                                WriteToLog(ex.StackTrace);
                            }
                        }
                        return;
                    }

                    // Analyzing
                    // analyze: 100%
                    else if (Regex.IsMatch(data, "^analyze: [0-9]{1,3}%$", RegexOptions.Compiled))
                    {
                        if (backgroundWorker.IsBusy)
                            backgroundWorker.ReportProgress(int.Parse(Regex.Match(data, "[0-9]{1,3}").Value),
                                string.Format("Analyzing ({0}%)", int.Parse(Regex.Match(data, "[0-9]{1,3}").Value)));

                        return;
                    }

                    // Information line
                    // [a03] Creating file "audio.ac3"...
                    else if (Regex.IsMatch(data, @"^\[.+\] .+\.{3}$", RegexOptions.Compiled))
                    {
                        WriteToLog(data);
                        return;
                    }

                    else if (Regex.IsMatch(data, @"^\v .*...", RegexOptions.Compiled))
                    {
                        WriteToLog(data);
                        return;
                    }

                    else if (Regex.IsMatch(data, @"(core: .*)", RegexOptions.Compiled))
                    {
                        //WriteToLog(data);
                        return;
                    }

                    else if (Regex.IsMatch(data, @"(embedded: .*)", RegexOptions.Compiled))
                    {
                        //WriteToLog(data);
                        return;
                    }

                    // Creating file
                    // Creating file "C:\1_1_chapter.txt"...
                    else if (Regex.IsMatch(data, "^Creating file \".+\"\\.{3}$", RegexOptions.Compiled))
                    {
                        WriteToLog(data);
                        return;
                    }

                    // Processing
                    // process: 100%
                    else if (Regex.IsMatch(data, "^process: [0-9]{1,3}%$", RegexOptions.Compiled))
                    {
                        if (backgroundWorker.IsBusy)
                            backgroundWorker.ReportProgress(int.Parse(Regex.Match(data, "[0-9]{1,3}").Value),
                                string.Format("Processing ({0}%)", int.Parse(Regex.Match(data, "[0-9]{1,3}").Value)));

                        return;
                    }

                    // Progress
                    // progress: 100%
                    else if (Regex.IsMatch(data, "^progress: [0-9]{1,3}%$", RegexOptions.Compiled))
                    {
                        if (backgroundWorker.IsBusy)
                            backgroundWorker.ReportProgress(int.Parse(Regex.Match(data, "[0-9]{1,3}").Value),
                                string.Format("Progress ({0}%)", int.Parse(Regex.Match(data, "[0-9]{1,3}").Value)));

                        return;
                    }

                    // Done
                    // Done.
                    else if (data.Equals("Done."))
                    {
                        WriteToLog(data);
                        return;
                    }

                    #region Errors
                    // Source file not found
                    // Source file "x:\" not found.
                    else if (Regex.IsMatch(data, "^Source file \".*\" not found.$", RegexOptions.Compiled))
                    {
                        MessageBox.Show(data, "Source", MessageBoxButtons.OK, MessageBoxIcon.Error, MessageBoxDefaultButton.Button1);
                        WriteToLog(data);
                        return;
                    }

                    // Format of Source file not detected
                    // The format of the source file could not be detected.
                    else if (data.Equals("The format of the source file could not be detected."))
                    {
                        MessageBox.Show(data, "Source File Format", MessageBoxButtons.OK, MessageBoxIcon.Error, MessageBoxDefaultButton.Button1);
                        WriteToLog(data);
                        return;
                    }

                    // Audio conversion not supported
                    // This audio conversion is not supported.
                    else if (data.Equals("This audio conversion is not supported."))
                    {
                        MessageBox.Show(data, "Audio Conversion", MessageBoxButtons.OK, MessageBoxIcon.Error, MessageBoxDefaultButton.Button1);
                        WriteToLog(data);
                        return;
                    }
                    #endregion

                    // Unknown line
                    else
                    {
                        WriteToLog(string.Format("Unknown line: \"{0}\"", data));
                    }
                }
            }
        }
        #endregion

        #region GUI
        delegate void SetToolStripProgressBarValueCallback(int value);
        private void SetToolStripProgressBarValue(int value)
        {
            lock (this)
            {
                if (this.InvokeRequired)
                    this.BeginInvoke(new SetToolStripProgressBarValueCallback(SetToolStripProgressBarValue), value);
                else
                    this.ToolStripProgressBar.Value = value;
            }
        }

        delegate void SetToolStripLabelTextCallback(string message);
        private void SetToolStripLabelText(string message)
        {
            lock (this)
            {
                if (this.InvokeRequired)
                    this.BeginInvoke(new SetToolStripLabelTextCallback(SetToolStripLabelText), message);
                else
                    this.ToolStripStatusLabel.Text = message;
            }
        }

        delegate void ResetCursorCallback(System.Windows.Forms.Cursor cursor);
        private void ResetCursor(System.Windows.Forms.Cursor cursor)
        {
            lock (this)
            {
                if (this.InvokeRequired)
                    this.BeginInvoke(new ResetCursorCallback(ResetCursor), cursor);
                else
                    this.Cursor = cursor;
            }
        }

        delegate void WriteToLogCallback(string text);
        private void WriteToLog(string text)
        {
            if (LogTextBox.InvokeRequired)
                LogTextBox.BeginInvoke(new WriteToLogCallback(WriteToLog), text);
            else
            {
                lock (this)
                {
                    LogTextBox.AppendText(string.Format("[{0}] {1}{2}", DateTime.Now.ToString("HH:mm:ss"), text, Environment.NewLine));

                    using (System.IO.StreamWriter SW = new System.IO.StreamWriter(System.IO.Path.Combine(System.IO.Directory.GetCurrentDirectory(), "HdBrStreamExtractor.txt"), true))
                    {
                        SW.WriteLine(string.Format("[{0}] {1}{2}", DateTime.Now.ToString("HH:mm:ss"), text, Environment.NewLine));
                        SW.Close();
                    }
                }
            }
        }

        private void FolderInputSourceButton_Click(object sender, EventArgs e)
        {
            string myinput = "";
            DialogResult dr;
            int idx = 0;

            if (FolderSelection.Checked)
            {
                folderBrowserDialog1.SelectedPath = MainForm.Instance.Settings.LastSourcePath;
                folderBrowserDialog1.Description = "Choose an input directory";
                folderBrowserDialog1.ShowNewFolderButton = false;
                dr = folderBrowserDialog1.ShowDialog();
                if (folderBrowserDialog1.SelectedPath.EndsWith(":\\"))
                     myinput = folderBrowserDialog1.SelectedPath;
                else myinput = folderBrowserDialog1.SelectedPath + System.IO.Path.DirectorySeparatorChar;
                if (dr == DialogResult.OK)
                    MainForm.Instance.Settings.LastSourcePath = myinput;
            }
            else
            {
                dr = openFileDialog1.ShowDialog();
                inputType = 2; 
                foreach (String file in openFileDialog1.FileNames)
                {
                    if (idx > 0) // seamless branching
                         myinput += "+" + file;
                    else myinput = file;
                    idx++;
                }
            }

            if (dr == DialogResult.OK)
                FolderInputTextBox.Text = myinput;

            if (FolderInputTextBox.Text != "")
            {
                string projectPath;
                if (!string.IsNullOrEmpty(projectPath = MainForm.Instance.Settings.DefaultOutputDir))
                    FolderOutputTextBox.Text = projectPath;
                else
                {
                    if (string.IsNullOrEmpty(FolderOutputTextBox.Text))
                    {
                        if (idx > 0) // seamless branching
                            FolderOutputTextBox.Text = FolderInputTextBox.Text.Substring(0, (FolderInputTextBox.Text.LastIndexOf("\\") - FolderInputTextBox.Text.LastIndexOf("+")));
                        else
                            FolderOutputTextBox.Text = FolderInputTextBox.Text.Substring(0, FolderInputTextBox.Text.LastIndexOf("\\") + 1);
                    }
                }
                if (!System.IO.File.Exists(settings.EAC3toPath))
                {
                    MessageBox.Show("EAC3to not found. Please use the updater.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    return;
                }
                else
                    FeatureButton_Click(null, null);
            }
        }

        private void FolderOutputSourceButton_Click(object sender, EventArgs e)
        {
            folderBrowserDialog1.SelectedPath = MainForm.Instance.Settings.LastDestinationPath;
            folderBrowserDialog1.Description = "Choose an output directory";
            folderBrowserDialog1.ShowNewFolderButton = true;
            DialogResult dr = folderBrowserDialog1.ShowDialog();

            if (dr == DialogResult.OK)
            {
                FolderOutputTextBox.Text = folderBrowserDialog1.SelectedPath;
                MainForm.Instance.Settings.LastDestinationPath = folderBrowserDialog1.SelectedPath;
            }
        }

        private void FeatureButton_Click(object sender, EventArgs e)
        {
            if (string.IsNullOrEmpty(FolderInputTextBox.Text))
            {
                MessageBox.Show("Configure input source folder prior to retrieving features.", "Feature Retrieval", MessageBoxButtons.OK, MessageBoxIcon.Exclamation, MessageBoxDefaultButton.Button1);
            }
            else
            {
                InitBackgroundWorker();
                eac3toArgs args = new eac3toArgs();

                args.eac3toPath = eac3toPath;
                args.inputPath = FolderInputTextBox.Text;
                args.workingFolder = string.IsNullOrEmpty(FolderOutputTextBox.Text) ? FolderOutputTextBox.Text : System.IO.Path.GetDirectoryName(args.eac3toPath);
                if (FolderSelection.Checked)
                {
                    args.resultState = ResultState.FeatureCompleted;
                    args.args = string.Empty;

                    features = new List<Feature>();
                    backgroundWorker.ReportProgress(0, "Retrieving features...");
                    WriteToLog("Retrieving features...");
                }
                else
                {
                    args.resultState = ResultState.StreamCompleted;
                    args.args = string.Empty;

                    streams = new List<Stream>();
                    backgroundWorker.ReportProgress(0, "Retrieving streams...");
                    WriteToLog("Retrieving streams...");
 
                }
                FeatureButton.Enabled = false;
                Cursor = Cursors.WaitCursor;

                backgroundWorker.RunWorkerAsync(args);
            }
        }

        private void StreamDataGridView_DataSourceChanged(object sender, EventArgs e)
        {
            foreach (DataGridViewRow row in StreamDataGridView.Rows)
            {
                Stream s = row.DataBoundItem as Stream;
                DataGridViewComboBoxCell comboBox = row.Cells["StreamExtractAsComboBox"] as DataGridViewComboBoxCell;
                DataGridViewTextBoxCell tbLang = row.Cells["languageDataGridViewTextBoxColumn"] as DataGridViewTextBoxCell;
                comboBox.Items.Clear();
                comboBox.Items.AddRange(s.ExtractTypes);

                switch (s.Type)
                {
                    case eac3to.StreamType.Chapter:
                        comboBox.Value = "TXT";
                        break;
                    case eac3to.StreamType.Join:
                        if (s.Name == "Joined EVO")
                             comboBox.Value = "EVO";
                        else comboBox.Value = "VOB";
                        break;
                    case eac3to.StreamType.Subtitle:
                        switch (s.Description.Substring(11, 3))
                        {
                            case "ASS": comboBox.Value = "ASS"; break;
                            case "SSA": comboBox.Value = "SSA"; break;
                            case "SRT": comboBox.Value = "SRT"; break;
                            case "Vob": comboBox.Value = "IDX"; break;
                            default: comboBox.Value = "SUP"; break;
                        }
                        break;
                    case eac3to.StreamType.Video:
                        comboBox.Value = "MKV";
                        break;
                    case eac3to.StreamType.Audio:
                        comboBox.Value = comboBox.Items[0];
                        break;
                }

                if ((s.Type == eac3to.StreamType.Audio) || (s.Type == eac3to.StreamType.Subtitle))
                {
                    char[] separator = { ',' };
                    string[] split = s.Description.Split(separator, 100);

                    if (s.Name.Contains("Subtitle"))
                        s.Language = s.Name;
                    else s.Language = split[1].Substring(1, split[1].Length - 1);
                }
                else s.Language = "";

            }
        }

        private void InitBackgroundWorker()
        {
            backgroundWorker = new BackgroundWorker();
            backgroundWorker.WorkerSupportsCancellation = true;
            backgroundWorker.WorkerReportsProgress = true;
            backgroundWorker.DoWork += new DoWorkEventHandler(backgroundWorker_DoWork);
            backgroundWorker.ProgressChanged += new ProgressChangedEventHandler(backgroundWorker_ProgressChanged);
            backgroundWorker.RunWorkerCompleted += new RunWorkerCompletedEventHandler(backgroundWorker_RunWorkerCompleted);
        }

        private void QueueButton_Click(object sender, EventArgs e)
        {
            if (string.IsNullOrEmpty(FolderOutputTextBox.Text))
            {
                MessageBox.Show("Configure output target folder prior to enqueueing job.", "Enqueue Job", MessageBoxButtons.OK, MessageBoxIcon.Exclamation, MessageBoxDefaultButton.Button1);
                return;
            }

            if (StreamDataGridView.Rows.Count == 0)
            {
                MessageBox.Show("Retrieve streams prior to enqueueing job.", "Enqueue Job", MessageBoxButtons.OK, MessageBoxIcon.Exclamation, MessageBoxDefaultButton.Button1);
                return;
            }

            if (!IsStreamCheckedForExtract())
            {
                MessageBox.Show("Select stream(s) to extract prior to enqueueing job.", "Enqueue Job", MessageBoxButtons.OK, MessageBoxIcon.Exclamation, MessageBoxDefaultButton.Button1);
                return;
            }

            if (!Drives.ableToWriteOnThisDrive(System.IO.Path.GetPathRoot(FolderOutputTextBox.Text)))
            {
                MessageBox.Show("MeGUI cannot write on " + System.IO.Path.GetPathRoot(FolderOutputTextBox.Text) +
                                "\nPlease, select another Output path.", "Information", MessageBoxButtons.OK, MessageBoxIcon.Information);
                return;
            }

            if ((settings.EAC3toPath == "") || (settings.EAC3toPath == "eac3to.exe"))
            {
                MessageBox.Show("Select a correct EAC3to Path first in the MeGUI Settings to avoid issues...", "Information", MessageBoxButtons.OK, MessageBoxIcon.Information);
                return;
            }

            eac3toArgs args = new eac3toArgs();
            HDStreamsExJob job;
           
            args.eac3toPath = eac3toPath;
            args.inputPath = FolderInputTextBox.Text;
            if (FolderSelection.Checked)
            {
                if (seamless)
                    args.featureNumber = "1"; // force the feature number
                else
                    args.featureNumber = ((Feature)FeatureDataGridView.SelectedRows[0].DataBoundItem).Number.ToString();
            }
            args.workingFolder = string.IsNullOrEmpty(FolderOutputTextBox.Text) ? FolderOutputTextBox.Text : System.IO.Path.GetDirectoryName(args.eac3toPath);
            args.resultState = ResultState.ExtractCompleted;

            try
            {
                args.args = GenerateArguments();
            }
            catch (ApplicationException ex)
            {
                MessageBox.Show(ex.Message, "Stream Extract", MessageBoxButtons.OK, MessageBoxIcon.Exclamation, MessageBoxDefaultButton.Button1);
                return;
            }

/*
            InitBackgroundWorker();
            backgroundWorker.ReportProgress(0, "Extracting streams");
            WriteToLog("Extracting streams");
            QueueButton.Enabled = false;
            Cursor = Cursors.WaitCursor;

            backgroundWorker.RunWorkerAsync(args);*/

            // Load to MeGUI job queue
            if (FolderSelection.Checked)
                job = new HDStreamsExJob(dummyInput, this.FolderOutputTextBox.Text+"xxx", args.featureNumber, args.args, inputType);
            else job = new HDStreamsExJob(this.FolderInputTextBox.Text, this.FolderOutputTextBox.Text+"xxx", null, args.args, inputType);

            lastJob = job;
            info.Jobs.addJobsToQueue(job);
            if (this.closeOnQueue.Checked)
                this.Close();
        }

        public HDStreamsExJob LastJob
        {
            get { return lastJob; }
            set { lastJob = value; }
        }
        public bool JobCreated
        {
            get { return lastJob != null; }
        }

        private string GenerateArguments()
        {
            StringBuilder sb = new StringBuilder();

            foreach (DataGridViewRow row in StreamDataGridView.Rows)
            {
                Stream stream = row.DataBoundItem as Stream;
                DataGridViewCheckBoxCell extractStream = row.Cells["StreamExtractCheckBox"] as DataGridViewCheckBoxCell;

                if (extractStream.Value != null && int.Parse(extractStream.Value.ToString()) == 1)
                {
                    if (row.Cells["StreamExtractAsComboBox"].Value == null)
                        throw new ApplicationException(string.Format("Specify an extraction type for stream:\r\n\n\t{0}: {1}", stream.Number, stream.Name));

                    if (FolderSelection.Checked)
                        sb.Append(string.Format("{0}:\"{1}\" {2} ", stream.Number,
                            System.IO.Path.Combine(FolderOutputTextBox.Text, string.Format("F{0}_T{1}_{2} - {3}.{4}", ((Feature)FeatureDataGridView.SelectedRows[0].DataBoundItem).Number, stream.Number, Extensions.GetStringValue(stream.Type), row.Cells["languageDataGridViewTextBoxColumn"].Value, (row.Cells["StreamExtractAsComboBox"].Value).ToString().ToLower())),
                            row.Cells["StreamAddOptionsTextBox"].Value).Trim());
                    else
                        sb.Append(string.Format("{0}:\"{1}\" {2} ", stream.Number,
                            System.IO.Path.Combine(FolderOutputTextBox.Text, string.Format("T{0}_{1} - {2}.{3}", stream.Number, Extensions.GetStringValue(stream.Type), row.Cells["languageDataGridViewTextBoxColumn"].Value, (row.Cells["StreamExtractAsComboBox"].Value).ToString().ToLower())),
                            row.Cells["StreamAddOptionsTextBox"].Value).Trim());

                    if (row.Cells["StreamExtractAsComboBox"].Value.Equals(AudioCodec.DTS.ID))
                        sb.Append(" -core");

                    sb.Append(" ");
                }
            }

            return sb.ToString();
        }

        private void CancelButton2_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void HelpButton2_Click(object sender, EventArgs e)
        {
            System.Diagnostics.Process.Start("http://en.wikibooks.org/wiki/Eac3to/How_to_Use");
        }

        private void HdBrStreamExtractor_FormClosing(object sender, System.Windows.Forms.FormClosingEventArgs e)
        {
            if (backgroundWorker != null)
            {
                if (backgroundWorker.IsBusy)
                    if (MessageBox.Show("A process is still running. Do you want to cancel it?", "Cancel process?", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
                        backgroundWorker.CancelAsync();

                if (backgroundWorker.CancellationPending)
                    backgroundWorker.Dispose();
            }
        }

        private bool IsStreamCheckedForExtract()
        {
            bool enableQueue = false;

            foreach (DataGridViewRow row in StreamDataGridView.Rows)
                if (row.Cells["StreamExtractCheckBox"].Value != null && int.Parse(row.Cells["StreamExtractCheckBox"].Value.ToString()) == 1)
                    enableQueue = true;

            return enableQueue;
        }

        private void Eac3toLinkLabel_LinkClicked(object sender, System.Windows.Forms.LinkLabelLinkClickedEventArgs e)
        {
            System.Diagnostics.Process.Start("http://forum.doom9.org/showthread.php?t=125966");
        }

        private void FeatureDataGridView_SelectionChanged(object sender, EventArgs e)
        {
            // only fire after the Databind has completed on grid and a row is selected
            if (FeatureDataGridView.Rows.Count == features.Count && FeatureDataGridView.SelectedRows.Count == 1)
            {
                if (backgroundWorker.IsBusy) // disallow selection change
                {
                    this.FeatureDataGridView.SelectionChanged -= new System.EventHandler(this.FeatureDataGridView_SelectionChanged);

                    FeatureDataGridView.CurrentRow.Selected = false;
                    FeatureDataGridView.Rows[int.Parse(FeatureDataGridView.Tag.ToString())].Selected = true;

                    this.FeatureDataGridView.SelectionChanged += new System.EventHandler(this.FeatureDataGridView_SelectionChanged);
                }
                else // backgroundworker is not busy, allow selection change
                {
                    Feature feature = FeatureDataGridView.SelectedRows[0].DataBoundItem as Feature;

                    // Check for Streams
                    if (feature.Streams == null || feature.Streams.Count == 0)
                    {
                        InitBackgroundWorker();
                        eac3toArgs args = new eac3toArgs();

                        args.eac3toPath = eac3toPath;
                        args.inputPath = FolderInputTextBox.Text;
                        args.workingFolder = string.IsNullOrEmpty(FolderOutputTextBox.Text) ? FolderOutputTextBox.Text : System.IO.Path.GetDirectoryName(args.eac3toPath);
                        args.resultState = ResultState.StreamCompleted;
                        args.args = ((Feature)FeatureDataGridView.SelectedRows[0].DataBoundItem).Number.ToString();

                        // create dummy input string for megui job
                        if (feature.Description.Contains("EVO"))
                        {
                            if (args.inputPath.ToUpper().Contains("HVDVD_TS"))
                                 dummyInput = args.inputPath + feature.Description.Substring(0, feature.Description.IndexOf(","));
                            else dummyInput = args.inputPath + "HVDVD_TS\\" + feature.Description.Substring(0, feature.Description.IndexOf(","));
                        }
                        else if (feature.Description.Contains("(angle"))
                        {
                            if (args.inputPath.ToUpper().Contains("BDMV\\PLAYLIST"))
                                 dummyInput = args.inputPath + feature.Description.Substring(0, feature.Description.IndexOf(" ("));
                            else if (args.inputPath.ToUpper().Contains("BDMV\\STREAM"))
                                 dummyInput = args.inputPath.Substring(0, args.inputPath.LastIndexOf("BDMV")) + "BDMV\\PLAYLIST\\" + feature.Description.Substring(0, feature.Description.IndexOf(" ("));
                            else dummyInput = args.inputPath + "BDMV\\PLAYLIST\\" + feature.Description.Substring(0, feature.Description.IndexOf(" ("));
                        }
                        else if (feature.Description.Substring(feature.Description.LastIndexOf(".") + 1, 4) == "m2ts")
                        {
                            string des = feature.Description.Substring(feature.Description.IndexOf(",") + 2, feature.Description.LastIndexOf(",") - feature.Description.IndexOf(",") - 2); 

                            if (des.Contains("+")) // seamless branching
                            {
                                seamless = true;
                                if (args.inputPath.ToUpper().Contains("BDMV\\STREAM"))
                                     dummyInput = args.inputPath.Substring(0, args.inputPath.IndexOf("BDMV")) + "BDMV\\PLAYLIST\\" + feature.Description.Substring(0, feature.Description.IndexOf(","));
                                else
                                     dummyInput = args.inputPath + "BDMV\\PLAYLIST\\" + feature.Description.Substring(0, feature.Description.IndexOf(","));
                            }
                            else
                            {
                                if (args.inputPath.ToUpper().Contains("BDMV\\STREAM"))
                                     dummyInput = args.inputPath + des;
                                else dummyInput = args.inputPath + "BDMV\\STREAM\\" + des;
                            }
                        }
                        else
                        {
                            if (args.inputPath.ToUpper().Contains("BDMV\\PLAYLIST"))
                                 dummyInput = args.inputPath + feature.Description.Substring(0, feature.Description.IndexOf(","));
                            else dummyInput = args.inputPath + "BDMV\\PLAYLIST\\" + feature.Description.Substring(0, feature.Description.IndexOf(","));
                        }

                        backgroundWorker.ReportProgress(0, "Retrieving streams...");
                        WriteToLog("Retrieving streams...");
                        Cursor = Cursors.WaitCursor;

                        backgroundWorker.RunWorkerAsync(args);
                    }
                    else // use already collected streams
                    {
                        StreamDataGridView.DataSource = feature.Streams;
                    }
                }
            }
        }

        private void FeatureDataGridView_DataBindingComplete(object sender, System.Windows.Forms.DataGridViewBindingCompleteEventArgs e)
        {
            FeatureDataGridView.ClearSelection();
        }

        private void FeatureDataGridView_RowLeave(object sender, System.Windows.Forms.DataGridViewCellEventArgs e)
        {
            FeatureDataGridView.Tag = e.RowIndex;
        }

        private void FeatureDataGridView_DataSourceChanged(object sender, EventArgs e)
        {
            foreach (DataGridViewRow row in FeatureDataGridView.Rows)
            {
                Feature feature = row.DataBoundItem as Feature;
                DataGridViewComboBoxCell comboBox = row.Cells["FeatureFileDataGridViewComboBoxColumn"] as DataGridViewComboBoxCell;

                if (feature != null)
                {
                    if (feature.Files != null || feature.Files.Count > 0)
                    {
                        foreach (File file in feature.Files)
                        {
                            comboBox.Items.Add(file.FullName);

                            if (file.Index == 1)
                                comboBox.Value = file.FullName;
                        }
                    }
                }
            }
        }
        #endregion
    }

    public class HdBdExtractorTool : ITool
    {

        #region ITool Members

        public string Name
        {
            get { return "HD Streams Extractor"; }
        }

        public void Run(MainForm info)
        {
            (new HdBdStreamExtractor(info)).Show();
        }

        public Shortcut[] Shortcuts
        {
            get { return new Shortcut[] { Shortcut.CtrlF7}; }
        }

        #endregion

        #region IIDable Members

        public string ID
        {
            get { return Name; }
        }

        #endregion
    }
}
