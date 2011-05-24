namespace ProcessHacker.Components
{
    partial class TokenGroupsList
    {



        private System.ComponentModel.IContainer components = null;





        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }







        private void InitializeComponent()
        {
            this.listGroups = new System.Windows.Forms.ListView();
            this.columnGroupName = new System.Windows.Forms.ColumnHeader();
            this.columnFlags = new System.Windows.Forms.ColumnHeader();
            this.SuspendLayout();



            this.listGroups.AllowColumnReorder = true;
            this.listGroups.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnGroupName,
            this.columnFlags});
            this.listGroups.Dock = System.Windows.Forms.DockStyle.Fill;
            this.listGroups.FullRowSelect = true;
            this.listGroups.Location = new System.Drawing.Point(0, 0);
            this.listGroups.Name = "listGroups";
            this.listGroups.ShowItemToolTips = true;
            this.listGroups.Size = new System.Drawing.Size(431, 397);
            this.listGroups.Sorting = System.Windows.Forms.SortOrder.Ascending;
            this.listGroups.TabIndex = 4;
            this.listGroups.UseCompatibleStateImageBehavior = false;
            this.listGroups.View = System.Windows.Forms.View.Details;



            this.columnGroupName.Text = "Name";
            this.columnGroupName.Width = 200;



            this.columnFlags.Text = "Flags";
            this.columnFlags.Width = 180;



            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.listGroups);
            this.Name = "TokenGroups";
            this.Size = new System.Drawing.Size(431, 397);
            this.ResumeLayout(false);

        }



        private System.Windows.Forms.ListView listGroups;
        private System.Windows.Forms.ColumnHeader columnGroupName;
        private System.Windows.Forms.ColumnHeader columnFlags;
    }
}
