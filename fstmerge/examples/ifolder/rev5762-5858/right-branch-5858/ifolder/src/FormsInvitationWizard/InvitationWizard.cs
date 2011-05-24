

using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;
using System.IO;
using System.Reflection;
using Simias;
using Simias.Sync;
using Simias.POBox;
using Simias.Storage;

using System.Text;

namespace Novell.InvitationWizard
{



 [Flags]
 public enum WizardButtons
 {



  None = 0,




  Back = 1,




  Next = 2,




  Cancel = 4,
 }




 public class InvitationWizard : System.Windows.Forms.Form
 {

  private static readonly ISimiasLog logger = SimiasLogManager.GetLogger(typeof(InvitationWizard));
  private System.Windows.Forms.Button cancel;
  private System.Windows.Forms.Button next;
  private System.Windows.Forms.Button back;
  internal System.Windows.Forms.GroupBox groupBox1;
  private Novell.InvitationWizard.WelcomePage welcomePage;
  private SelectInvitationPage selectInvitationPage;
  private AcceptDeclinePage acceptDeclinePage;
  private SelectiFolderLocationPage selectiFolderLocationPage;
  private CompletionPage completionPage;
  private BaseWizardPage[] pages;
  internal const int maxPages = 5;
  private int currentIndex = 0;
  private Subscription subscription = null;
  private string invitationFile = "";
  private string domainID;
  private string subscriptionID;
  private bool subscriptionMode = false;
  private Store store;
  private POBox poBox = null;




  private System.ComponentModel.Container components = null;






  public InvitationWizard(string[] args)
  {



   InitializeComponent();



   this.welcomePage = new Novell.InvitationWizard.WelcomePage();
   this.selectInvitationPage = new Novell.InvitationWizard.SelectInvitationPage();
   this.acceptDeclinePage = new Novell.InvitationWizard.AcceptDeclinePage();
   this.selectiFolderLocationPage = new Novell.InvitationWizard.SelectiFolderLocationPage();
   this.completionPage = new CompletionPage();



   this.welcomePage.DescriptionText = "Use this wizard to accept or decline an invitation to participate in a shared iFolder. If you accept, it helps you place the iFolder in an appropriate location on this computer. If you decline, it revokes the invitation. The iFolder client uses your current Windows logon to establish local ownership of the shared iFolder.";
   this.welcomePage.ActionText = "Click Next to continue, or click Cancel to exit.";
   this.welcomePage.Location = new System.Drawing.Point(0, 0);
   this.welcomePage.Name = "welcomePage";
   this.welcomePage.Size = new System.Drawing.Size(496, 304);
   this.welcomePage.TabIndex = 1;
   this.welcomePage.WelcomeTitle = "Welcome to the iFolder Invitation Wizard";



   this.selectInvitationPage.HeaderSubTitle = "Specify the location of the iFolder Invitation to accept or decline.";
   this.selectInvitationPage.HeaderTitle = "Select an iFolder Invitation";
   this.selectInvitationPage.Location = new System.Drawing.Point(0, 0);
   this.selectInvitationPage.Name = "selectInvitationPage";
   this.selectInvitationPage.Size = new System.Drawing.Size(496, 304);
   this.selectInvitationPage.TabIndex = 1;



   this.acceptDeclinePage.HeaderSubTitle = "Accept or decline the invitation to participate in this shared iFolder.";
   this.acceptDeclinePage.HeaderTitle = "Respond to the iFolder Invitation";
   this.acceptDeclinePage.Location = new System.Drawing.Point(0, 0);
   this.acceptDeclinePage.Name = "acceptDeclinePage";
   this.acceptDeclinePage.Size = new System.Drawing.Size(496, 304);
   this.acceptDeclinePage.TabIndex = 1;



   this.selectiFolderLocationPage.HeaderSubTitle = "Specify the location on this computer to place the shared iFolder.";
   this.selectiFolderLocationPage.HeaderTitle = "Select iFolder Location";
   this.selectiFolderLocationPage.Location = new System.Drawing.Point(0, 0);
   this.selectiFolderLocationPage.Name = "selectiFolderLocationPage";
   this.selectiFolderLocationPage.Size = new System.Drawing.Size(496, 304);
   this.selectiFolderLocationPage.TabIndex = 1;



   this.completionPage.DescriptionText = "Description...";
   this.completionPage.Location = new System.Drawing.Point(0, 0);
   this.completionPage.Name = "completionPage";
   this.completionPage.Size = new System.Drawing.Size(496, 304);
   this.completionPage.TabIndex = 1;
   this.completionPage.WelcomeTitle = "Completing the iFolder Invitation Wizard";

   this.Controls.Add(this.welcomePage);
   this.Controls.Add(this.selectInvitationPage);
   this.Controls.Add(this.acceptDeclinePage);
   this.Controls.Add(this.selectiFolderLocationPage);
   this.Controls.Add(this.completionPage);


   try
   {
    this.Icon = new Icon(Path.Combine(Application.StartupPath, "Invitation.ico"));
   }
   catch (Exception e)
   {
    logger.Debug(e, "Loading icon");
   }


   pages = new BaseWizardPage[maxPages];
   pages[0] = this.welcomePage;
   pages[1] = this.selectInvitationPage;
   pages[2] = this.acceptDeclinePage;
   pages[3] = this.selectiFolderLocationPage;
   pages[4] = this.completionPage;

   try
   {

    Image image = Image.FromFile(Path.Combine(Application.StartupPath, "invitewiz.png"));
    this.welcomePage.Watermark = image;
    this.completionPage.Watermark = image;

    image = Image.FromFile(Path.Combine(Application.StartupPath, @"res\ifolder_invite_32.png"));
    this.selectInvitationPage.Thumbnail = image;
    this.selectiFolderLocationPage.Thumbnail = image;
    this.acceptDeclinePage.Thumbnail = image;
   }
   catch (Exception e)
   {
    logger.Debug(e, "Loading watermark");
   }

   foreach (BaseWizardPage page in pages)
   {
    page.Hide();
   }


   pages[0].ActivatePage(0);

   if (args.Length > 0)
   {
    string arg = "/ID=";
    if (args[0].StartsWith(arg))
    {
     subscriptionMode = true;

     string tmp = args[0].Substring(arg.Length);

     int index = tmp.IndexOf( ':' );
     if ( index != -1 )
     {
      subscriptionID = tmp.Substring( 0, index );
      domainID = tmp.Substring( index + 1 );
     }
    }
    else
    {
     this.invitationFile = args[0];
    }
   }



  }




  protected override void Dispose( bool disposing )
  {
   if( disposing )
   {
    if (components != null)
    {
     components.Dispose();
    }
   }
   base.Dispose( disposing );
  }






  private void InitializeComponent()
  {
   this.cancel = new System.Windows.Forms.Button();
   this.next = new System.Windows.Forms.Button();
   this.back = new System.Windows.Forms.Button();
   this.groupBox1 = new System.Windows.Forms.GroupBox();
   this.SuspendLayout();



   this.cancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
   this.cancel.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.cancel.Location = new System.Drawing.Point(416, 318);
   this.cancel.Name = "cancel";
   this.cancel.Size = new System.Drawing.Size(72, 23);
   this.cancel.TabIndex = 3;
   this.cancel.Text = "Cancel";
   this.cancel.Click += new System.EventHandler(this.cancel_Click);



   this.next.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.next.Location = new System.Drawing.Point(328, 318);
   this.next.Name = "next";
   this.next.Size = new System.Drawing.Size(72, 23);
   this.next.TabIndex = 2;
   this.next.Text = "Next >";
   this.next.Click += new System.EventHandler(this.next_Click);



   this.back.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.back.Location = new System.Drawing.Point(251, 318);
   this.back.Name = "back";
   this.back.Size = new System.Drawing.Size(72, 23);
   this.back.TabIndex = 1;
   this.back.Text = "< Back";
   this.back.Click += new System.EventHandler(this.back_Click);



   this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.groupBox1.Location = new System.Drawing.Point(0, 302);
   this.groupBox1.Name = "groupBox1";
   this.groupBox1.Size = new System.Drawing.Size(496, 4);
   this.groupBox1.TabIndex = 4;
   this.groupBox1.TabStop = false;
   this.groupBox1.Text = "groupBox1";



   this.AcceptButton = this.next;
   this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
   this.CancelButton = this.cancel;
   this.ClientSize = new System.Drawing.Size(496, 348);
   this.Controls.Add(this.groupBox1);
   this.Controls.Add(this.back);
   this.Controls.Add(this.next);
   this.Controls.Add(this.cancel);
   this.HelpButton = true;
   this.MaximizeBox = false;
   this.MinimizeBox = false;
   this.Name = "InvitationWizard";
   this.Text = "iFolder Invitation Wizard";
   this.Load += new System.EventHandler(this.InvitationWizard_Load);
   this.ResumeLayout(false);

  }





  [STAThread]
  static void Main(string[] args)
  {
   Application.Run(new InvitationWizard(args));
  }


  private void InvitationWizard_Load(object sender, System.EventArgs e)
  {
   store = Store.GetStore();

   if (subscriptionMode)
   {
    poBox = POBox.GetPOBox(store, domainID);
    Node node = poBox.GetNodeByID(subscriptionID);
    subscription = new Subscription(node);
   }
   else if (this.invitationFile != "")
   {
    try
    {
     subscription = Subscription.GetSubscriptionFromSubscriptionInfo(store, invitationFile);

     if (store.GetCollectionByID(subscription.SubscriptionCollectionID) != null)
     {
      MessageBox.Show("The collection for the selected subscription already exists.", "Shared Collection Exists");
      subscription = null;
      invitationFile = "";
     }
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An invalid Collection Subscription Information file was specified on the command-line.  Please see the log file for additional information.\n\n" + this.invitationFile, "Invalid File", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
     invitationFile = "";
    }
    catch (Exception ex)
    {

     logger.Debug(ex, "Invalid file");
     MessageBox.Show("An invalid Collection Subscription Information file was specified on the command-line.  Please see the log file for additional information.\n\n" + this.invitationFile, "Invalid File", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
     invitationFile = "";
    }
   }
  }

  private void next_Click(object sender, System.EventArgs e)
  {

   if (currentIndex == (maxPages - 1))
   {
    Cursor.Current = Cursors.WaitCursor;


    try
    {
     switch (subscription.SubscriptionState)
     {
      case SubscriptionStates.Received:
       subscription.Accept(store, acceptDeclinePage.Accept ? SubscriptionDispositions.Accepted : SubscriptionDispositions.Declined);
       if (poBox == null)
       {
        poBox = POBox.GetPOBox(store, subscription.DomainID);
       }
       break;
      case SubscriptionStates.Ready:
       subscription.CreateSlave(store);
       break;
     }

     poBox.Commit(subscription);
    }
    catch (SimiasException ex)
    {
     ex.LogFatal();
     MessageBox.Show("An exception occurred while accepting the iFolder invitation.  Please view the log file for additional information.", "Fatal Error", MessageBoxButtons.OK, MessageBoxIcon.Stop);
    }
    catch (Exception ex)
    {
     logger.Fatal(ex, "Accepting invitation");
     MessageBox.Show("An exception occurred while accepting the iFolder invitation.  Please view the log file for additional information.", "Fatal Error", MessageBoxButtons.OK, MessageBoxIcon.Stop);
    }

    Cursor.Current = Cursors.Default;


    Application.Exit();
    return;
   }

   int nextIndex = this.pages[currentIndex].ValidatePage(currentIndex);
   if (nextIndex != currentIndex)
   {
    this.pages[currentIndex].DeactivatePage();
    this.pages[nextIndex].ActivatePage(currentIndex);

    currentIndex = nextIndex;

    if (currentIndex == (maxPages - 1))
    {


     this.next.Text = "Finish";
    }
   }
  }

  private void back_Click(object sender, System.EventArgs e)
  {

   if (currentIndex == (maxPages - 1))
   {
    this.next.Text = "Next >";
   }

   int previousIndex = this.pages[currentIndex].DeactivatePage();
   this.pages[previousIndex].ActivatePage(0);

   currentIndex = previousIndex;
  }

  private void cancel_Click(object sender, System.EventArgs e)
  {
   Application.Exit();
  }






  public WizardButtons WizardButtons
  {
   get
   {
    WizardButtons wizardButtons = WizardButtons.None;
    wizardButtons |= cancel.Enabled ? WizardButtons.Cancel : WizardButtons.None;
    wizardButtons |= next.Enabled ? WizardButtons.Next : WizardButtons.None;
    wizardButtons |= back.Enabled ? WizardButtons.Back : WizardButtons.None;
    return wizardButtons;
   }
   set
   {
    WizardButtons wizardButtons = value;
    cancel.Enabled = ((wizardButtons & WizardButtons.Cancel) == WizardButtons.Cancel);
    next.Enabled = ((wizardButtons & WizardButtons.Next) == WizardButtons.Next);
    back.Enabled = ((wizardButtons & WizardButtons.Back) == WizardButtons.Back);
   }
  }




  public Subscription Subscription
  {
   get
   {
    return subscription;
   }

   set
   {
    subscription = value;
   }
  }




  public int MaxPages
  {
   get
   {
    return maxPages;
   }
  }




  public string InvitationFile
  {
   get
   {
    return invitationFile;
   }
  }




  public string SummaryText
  {
   get
   {

    StringBuilder sb = new StringBuilder("The wizard is ready to respond to the iFolder Invitation.\n\n");
    if (this.acceptDeclinePage.Accept)
    {
     sb.AppendFormat("You accepted the invitation to participate in the {0} iFolder shared by {1}.\n\n", subscription.SubscriptionCollectionName, subscription.FromName);
     if (Subscription.HasDirNode)
     {
      sb.AppendFormat("The planned location for the shared iFolder is {0}.", Path.Combine(subscription.CollectionRoot, subscription.SubscriptionCollectionName));
     }
    }
    else
    {
     sb.AppendFormat("You declined the invitation to participate in the {0} iFolder shared by {1}.\n\n", subscription.SubscriptionCollectionName, subscription.FromName);
     sb.Append("The invitation will be revoked and cannot be accepted at another computer.");
    }

    return sb.ToString();
   }
  }




  public Store Store
  {
   get { return store; }
  }




 }
}
