

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace ProcessHacker.Components
{
    public partial class ColorModifier : UserControl
    {
        public event EventHandler ColorChanged;

        private Color _color;

        public ColorModifier()
        {
            InitializeComponent();
        }

        private void panelColor_Click(object sender, EventArgs e)
        {
            ColorDialog cd = new ColorDialog();

            cd.Color = panelColor.BackColor;
            cd.FullOpen = true;

            if (cd.ShowDialog() == DialogResult.OK)
            {
                _color = cd.Color;
                panelColor.BackColor = cd.Color;

                if (this.ColorChanged != null)
                    this.ColorChanged(this, new EventArgs());
            }
        }

        public Color Color
        {
            get { return _color; }
            set
            {
                _color = value;
                panelColor.BackColor = value;

                if (this.ColorChanged != null)
                    this.ColorChanged(this, new EventArgs());
            }
        }

        private void panelColor_MouseEnter(object sender, EventArgs e)
        {
            panelColor.BackColor = Color.FromArgb(0xcc, _color);
        }

        private void panelColor_MouseLeave(object sender, EventArgs e)
        {
            panelColor.BackColor = _color;
        }
    }
}
