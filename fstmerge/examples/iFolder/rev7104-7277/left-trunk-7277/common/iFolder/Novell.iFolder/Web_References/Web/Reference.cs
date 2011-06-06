namespace Novell.iFolder.Web {
    using System.Diagnostics;
    using System.Web.Services;
    using System.ComponentModel;
    using System.Web.Services.Protocols;
    using System;
    using System.Xml.Serialization;
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    [System.Web.Services.WebServiceBindingAttribute(Name="iFolderWebServiceSoap", Namespace="http://novell.com/ifolder/web/")]
    public partial class iFolderWebService : System.Web.Services.Protocols.SoapHttpClientProtocol {
        private System.Threading.SendOrPostCallback PingOperationCompleted;
        private System.Threading.SendOrPostCallback IsiFolderOperationCompleted;
        private System.Threading.SendOrPostCallback CanBeiFolderOperationCompleted;
        private System.Threading.SendOrPostCallback IsPathIniFolderOperationCompleted;
        private System.Threading.SendOrPostCallback CreateLocaliFolderOperationCompleted;
        private System.Threading.SendOrPostCallback CreateiFolderInDomainEncrOperationCompleted;
        private System.Threading.SendOrPostCallback CreateiFolderInDomainOperationCompleted;
        private System.Threading.SendOrPostCallback CheckiFolderOperationCompleted;
        private System.Threading.SendOrPostCallback GetiFolderOperationCompleted;
        private System.Threading.SendOrPostCallback GetMinimaliFolderOperationCompleted;
        private System.Threading.SendOrPostCallback GetSecurityPolicyOperationCompleted;
        private System.Threading.SendOrPostCallback GetDisableSharingPolicyOperationCompleted;
        private System.Threading.SendOrPostCallback CanOwnerBeChangedOperationCompleted;
        private System.Threading.SendOrPostCallback GetLimitPolicyStatusOperationCompleted;
        private System.Threading.SendOrPostCallback GetiFolderInvitationOperationCompleted;
        private System.Threading.SendOrPostCallback GetiFolderByLocalPathOperationCompleted;
        private System.Threading.SendOrPostCallback DeleteiFolderOperationCompleted;
        private System.Threading.SendOrPostCallback RevertiFolderOperationCompleted;
        private System.Threading.SendOrPostCallback RevertiFolder1OperationCompleted;
        private System.Threading.SendOrPostCallback GetAlliFoldersOperationCompleted;
        private System.Threading.SendOrPostCallback GetAlliFolders1OperationCompleted;
        private System.Threading.SendOrPostCallback GetiFoldersForDomainOperationCompleted;
        private System.Threading.SendOrPostCallback GetiFoldersOperationCompleted;
        private System.Threading.SendOrPostCallback SetUserRightsOperationCompleted;
        private System.Threading.SendOrPostCallback GetOwnerOperationCompleted;
        private System.Threading.SendOrPostCallback ChangeOwnerOperationCompleted;
        private System.Threading.SendOrPostCallback RemoveiFolderUserOperationCompleted;
        private System.Threading.SendOrPostCallback GetiFolderUsersOperationCompleted;
        private System.Threading.SendOrPostCallback GetDomainUsersOperationCompleted;
        private System.Threading.SendOrPostCallback SearchForDomainUsersOperationCompleted;
        private System.Threading.SendOrPostCallback FindCloseiFolderMembersOperationCompleted;
        private System.Threading.SendOrPostCallback FindFirstiFolderMembersOperationCompleted;
        private System.Threading.SendOrPostCallback FindFirstSpecificiFolderMembersOperationCompleted;
        private System.Threading.SendOrPostCallback FindNextiFolderMembersOperationCompleted;
        private System.Threading.SendOrPostCallback FindPreviousiFolderMembersOperationCompleted;
        private System.Threading.SendOrPostCallback FindSeekiFolderMembersOperationCompleted;
        private System.Threading.SendOrPostCallback GetiFolderUserOperationCompleted;
        private System.Threading.SendOrPostCallback GetRANameOperationCompleted;
        private System.Threading.SendOrPostCallback GetiFolderUserFromNodeIDOperationCompleted;
        private System.Threading.SendOrPostCallback AddAndInviteUserOperationCompleted;
        private System.Threading.SendOrPostCallback InviteUserOperationCompleted;
        private System.Threading.SendOrPostCallback MergeiFolderOperationCompleted;
        private System.Threading.SendOrPostCallback AcceptiFolderInvitationOperationCompleted;
        private System.Threading.SendOrPostCallback AcceptiFolderInvitation1OperationCompleted;
        private System.Threading.SendOrPostCallback CheckForMacUpdateOperationCompleted;
        private System.Threading.SendOrPostCallback DeclineiFolderInvitationOperationCompleted;
        private System.Threading.SendOrPostCallback GetUserDiskSpaceOperationCompleted;
        private System.Threading.SendOrPostCallback GetiFolderDiskSpaceOperationCompleted;
        private System.Threading.SendOrPostCallback SetUserDiskSpaceLimitOperationCompleted;
        private System.Threading.SendOrPostCallback SetiFolderDiskSpaceLimitOperationCompleted;
        private System.Threading.SendOrPostCallback SetiFolderSecureSyncOperationCompleted;
        private System.Threading.SendOrPostCallback SetiFolderSyncIntervalOperationCompleted;
        private System.Threading.SendOrPostCallback SetDefaultSyncIntervalOperationCompleted;
        private System.Threading.SendOrPostCallback GetDefaultSyncIntervalOperationCompleted;
        private System.Threading.SendOrPostCallback AuthenticateToDomainOperationCompleted;
        private System.Threading.SendOrPostCallback GetiFolderConflictsOperationCompleted;
        private System.Threading.SendOrPostCallback ResolveFileConflictOperationCompleted;
        private System.Threading.SendOrPostCallback ResolveEnhancedFileConflictOperationCompleted;
        private System.Threading.SendOrPostCallback ResolveNameConflictOperationCompleted;
        private System.Threading.SendOrPostCallback RenameAndResolveConflictOperationCompleted;
        private System.Threading.SendOrPostCallback SetupProxyOperationCompleted;
        private System.Threading.SendOrPostCallback RemoveProxyOperationCompleted;
        private System.Threading.SendOrPostCallback CalculateSyncSizeOperationCompleted;
        private System.Threading.SendOrPostCallback SynciFolderNowOperationCompleted;
        private System.Threading.SendOrPostCallback DeleteiFolderFileSizeLimitOperationCompleted;
        private System.Threading.SendOrPostCallback GetMemberiFolderFileSizeLimitOperationCompleted;
        private System.Threading.SendOrPostCallback GetiFolderFileSizeLimitOperationCompleted;
        private System.Threading.SendOrPostCallback SetiFolderFileSizeLimitOperationCompleted;
        private System.Threading.SendOrPostCallback CheckForUpdatedClientAvailableOperationCompleted;
        private System.Threading.SendOrPostCallback CheckForUpdatedClientOperationCompleted;
        private System.Threading.SendOrPostCallback CheckForUpdateOperationCompleted;
        private System.Threading.SendOrPostCallback CheckForServerUpdateOperationCompleted;
        private System.Threading.SendOrPostCallback RunClientUpdateOperationCompleted;
        private System.Threading.SendOrPostCallback ChangePasswordOperationCompleted;
        private System.Threading.SendOrPostCallback GetDefaultServerPublicKeyOperationCompleted;
        private bool useDefaultCredentialsSetExplicitly;
        public iFolderWebService() {
            this.Url = global::Novell.iFolder.Properties.Settings.Default.Novell_iFolder_Web_iFolderWebService;
            if ((this.IsLocalFileSystemWebService(this.Url) == true)) {
                this.UseDefaultCredentials = true;
                this.useDefaultCredentialsSetExplicitly = false;
            }
            else {
                this.useDefaultCredentialsSetExplicitly = true;
            }
        }
        public new string Url {
            get {
                return base.Url;
            }
            set {
                if ((((this.IsLocalFileSystemWebService(base.Url) == true)
                            && (this.useDefaultCredentialsSetExplicitly == false))
                            && (this.IsLocalFileSystemWebService(value) == false))) {
                    base.UseDefaultCredentials = false;
                }
                base.Url = value;
            }
        }
        public new bool UseDefaultCredentials {
            get {
                return base.UseDefaultCredentials;
            }
            set {
                base.UseDefaultCredentials = value;
                this.useDefaultCredentialsSetExplicitly = true;
            }
        }
        public event PingCompletedEventHandler PingCompleted;
        public event IsiFolderCompletedEventHandler IsiFolderCompleted;
        public event CanBeiFolderCompletedEventHandler CanBeiFolderCompleted;
        public event IsPathIniFolderCompletedEventHandler IsPathIniFolderCompleted;
        public event CreateLocaliFolderCompletedEventHandler CreateLocaliFolderCompleted;
        public event CreateiFolderInDomainEncrCompletedEventHandler CreateiFolderInDomainEncrCompleted;
        public event CreateiFolderInDomainCompletedEventHandler CreateiFolderInDomainCompleted;
        public event CheckiFolderCompletedEventHandler CheckiFolderCompleted;
        public event GetiFolderCompletedEventHandler GetiFolderCompleted;
        public event GetMinimaliFolderCompletedEventHandler GetMinimaliFolderCompleted;
        public event GetSecurityPolicyCompletedEventHandler GetSecurityPolicyCompleted;
        public event GetDisableSharingPolicyCompletedEventHandler GetDisableSharingPolicyCompleted;
        public event CanOwnerBeChangedCompletedEventHandler CanOwnerBeChangedCompleted;
        public event GetLimitPolicyStatusCompletedEventHandler GetLimitPolicyStatusCompleted;
        public event GetiFolderInvitationCompletedEventHandler GetiFolderInvitationCompleted;
        public event GetiFolderByLocalPathCompletedEventHandler GetiFolderByLocalPathCompleted;
        public event DeleteiFolderCompletedEventHandler DeleteiFolderCompleted;
        public event RevertiFolderCompletedEventHandler RevertiFolderCompleted;
        public event RevertiFolder1CompletedEventHandler RevertiFolder1Completed;
        public event GetAlliFoldersCompletedEventHandler GetAlliFoldersCompleted;
        public event GetAlliFolders1CompletedEventHandler GetAlliFolders1Completed;
        public event GetiFoldersForDomainCompletedEventHandler GetiFoldersForDomainCompleted;
        public event GetiFoldersCompletedEventHandler GetiFoldersCompleted;
        public event SetUserRightsCompletedEventHandler SetUserRightsCompleted;
        public event GetOwnerCompletedEventHandler GetOwnerCompleted;
        public event ChangeOwnerCompletedEventHandler ChangeOwnerCompleted;
        public event RemoveiFolderUserCompletedEventHandler RemoveiFolderUserCompleted;
        public event GetiFolderUsersCompletedEventHandler GetiFolderUsersCompleted;
        public event GetDomainUsersCompletedEventHandler GetDomainUsersCompleted;
        public event SearchForDomainUsersCompletedEventHandler SearchForDomainUsersCompleted;
        public event FindCloseiFolderMembersCompletedEventHandler FindCloseiFolderMembersCompleted;
        public event FindFirstiFolderMembersCompletedEventHandler FindFirstiFolderMembersCompleted;
        public event FindFirstSpecificiFolderMembersCompletedEventHandler FindFirstSpecificiFolderMembersCompleted;
        public event FindNextiFolderMembersCompletedEventHandler FindNextiFolderMembersCompleted;
        public event FindPreviousiFolderMembersCompletedEventHandler FindPreviousiFolderMembersCompleted;
        public event FindSeekiFolderMembersCompletedEventHandler FindSeekiFolderMembersCompleted;
        public event GetiFolderUserCompletedEventHandler GetiFolderUserCompleted;
        public event GetRANameCompletedEventHandler GetRANameCompleted;
        public event GetiFolderUserFromNodeIDCompletedEventHandler GetiFolderUserFromNodeIDCompleted;
        public event AddAndInviteUserCompletedEventHandler AddAndInviteUserCompleted;
        public event InviteUserCompletedEventHandler InviteUserCompleted;
        public event MergeiFolderCompletedEventHandler MergeiFolderCompleted;
        public event AcceptiFolderInvitationCompletedEventHandler AcceptiFolderInvitationCompleted;
        public event AcceptiFolderInvitation1CompletedEventHandler AcceptiFolderInvitation1Completed;
        public event CheckForMacUpdateCompletedEventHandler CheckForMacUpdateCompleted;
        public event DeclineiFolderInvitationCompletedEventHandler DeclineiFolderInvitationCompleted;
        public event GetUserDiskSpaceCompletedEventHandler GetUserDiskSpaceCompleted;
        public event GetiFolderDiskSpaceCompletedEventHandler GetiFolderDiskSpaceCompleted;
        public event SetUserDiskSpaceLimitCompletedEventHandler SetUserDiskSpaceLimitCompleted;
        public event SetiFolderDiskSpaceLimitCompletedEventHandler SetiFolderDiskSpaceLimitCompleted;
        public event SetiFolderSecureSyncCompletedEventHandler SetiFolderSecureSyncCompleted;
        public event SetiFolderSyncIntervalCompletedEventHandler SetiFolderSyncIntervalCompleted;
        public event SetDefaultSyncIntervalCompletedEventHandler SetDefaultSyncIntervalCompleted;
        public event GetDefaultSyncIntervalCompletedEventHandler GetDefaultSyncIntervalCompleted;
        public event AuthenticateToDomainCompletedEventHandler AuthenticateToDomainCompleted;
        public event GetiFolderConflictsCompletedEventHandler GetiFolderConflictsCompleted;
        public event ResolveFileConflictCompletedEventHandler ResolveFileConflictCompleted;
        public event ResolveEnhancedFileConflictCompletedEventHandler ResolveEnhancedFileConflictCompleted;
        public event ResolveNameConflictCompletedEventHandler ResolveNameConflictCompleted;
        public event RenameAndResolveConflictCompletedEventHandler RenameAndResolveConflictCompleted;
        public event SetupProxyCompletedEventHandler SetupProxyCompleted;
        public event RemoveProxyCompletedEventHandler RemoveProxyCompleted;
        public event CalculateSyncSizeCompletedEventHandler CalculateSyncSizeCompleted;
        public event SynciFolderNowCompletedEventHandler SynciFolderNowCompleted;
        public event DeleteiFolderFileSizeLimitCompletedEventHandler DeleteiFolderFileSizeLimitCompleted;
        public event GetMemberiFolderFileSizeLimitCompletedEventHandler GetMemberiFolderFileSizeLimitCompleted;
        public event GetiFolderFileSizeLimitCompletedEventHandler GetiFolderFileSizeLimitCompleted;
        public event SetiFolderFileSizeLimitCompletedEventHandler SetiFolderFileSizeLimitCompleted;
        public event CheckForUpdatedClientAvailableCompletedEventHandler CheckForUpdatedClientAvailableCompleted;
        public event CheckForUpdatedClientCompletedEventHandler CheckForUpdatedClientCompleted;
        public event CheckForUpdateCompletedEventHandler CheckForUpdateCompleted;
        public event CheckForServerUpdateCompletedEventHandler CheckForServerUpdateCompleted;
        public event RunClientUpdateCompletedEventHandler RunClientUpdateCompleted;
        public event ChangePasswordCompletedEventHandler ChangePasswordCompleted;
        public event GetDefaultServerPublicKeyCompletedEventHandler GetDefaultServerPublicKeyCompleted;
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/Ping", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void Ping() {
            this.Invoke("Ping", new object[0]);
        }
        public void PingAsync() {
            this.PingAsync(null);
        }
        public void PingAsync(object userState) {
            if ((this.PingOperationCompleted == null)) {
                this.PingOperationCompleted = new System.Threading.SendOrPostCallback(this.OnPingOperationCompleted);
            }
            this.InvokeAsync("Ping", new object[0], this.PingOperationCompleted, userState);
        }
        private void OnPingOperationCompleted(object arg) {
            if ((this.PingCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.PingCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/IsiFolder", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public bool IsiFolder(string LocalPath) {
            object[] results = this.Invoke("IsiFolder", new object[] {
                        LocalPath});
            return ((bool)(results[0]));
        }
        public void IsiFolderAsync(string LocalPath) {
            this.IsiFolderAsync(LocalPath, null);
        }
        public void IsiFolderAsync(string LocalPath, object userState) {
            if ((this.IsiFolderOperationCompleted == null)) {
                this.IsiFolderOperationCompleted = new System.Threading.SendOrPostCallback(this.OnIsiFolderOperationCompleted);
            }
            this.InvokeAsync("IsiFolder", new object[] {
                        LocalPath}, this.IsiFolderOperationCompleted, userState);
        }
        private void OnIsiFolderOperationCompleted(object arg) {
            if ((this.IsiFolderCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.IsiFolderCompleted(this, new IsiFolderCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/CanBeiFolder", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public bool CanBeiFolder(string LocalPath) {
            object[] results = this.Invoke("CanBeiFolder", new object[] {
                        LocalPath});
            return ((bool)(results[0]));
        }
        public void CanBeiFolderAsync(string LocalPath) {
            this.CanBeiFolderAsync(LocalPath, null);
        }
        public void CanBeiFolderAsync(string LocalPath, object userState) {
            if ((this.CanBeiFolderOperationCompleted == null)) {
                this.CanBeiFolderOperationCompleted = new System.Threading.SendOrPostCallback(this.OnCanBeiFolderOperationCompleted);
            }
            this.InvokeAsync("CanBeiFolder", new object[] {
                        LocalPath}, this.CanBeiFolderOperationCompleted, userState);
        }
        private void OnCanBeiFolderOperationCompleted(object arg) {
            if ((this.CanBeiFolderCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.CanBeiFolderCompleted(this, new CanBeiFolderCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/IsPathIniFolder", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public bool IsPathIniFolder(string LocalPath) {
            object[] results = this.Invoke("IsPathIniFolder", new object[] {
                        LocalPath});
            return ((bool)(results[0]));
        }
        public void IsPathIniFolderAsync(string LocalPath) {
            this.IsPathIniFolderAsync(LocalPath, null);
        }
        public void IsPathIniFolderAsync(string LocalPath, object userState) {
            if ((this.IsPathIniFolderOperationCompleted == null)) {
                this.IsPathIniFolderOperationCompleted = new System.Threading.SendOrPostCallback(this.OnIsPathIniFolderOperationCompleted);
            }
            this.InvokeAsync("IsPathIniFolder", new object[] {
                        LocalPath}, this.IsPathIniFolderOperationCompleted, userState);
        }
        private void OnIsPathIniFolderOperationCompleted(object arg) {
            if ((this.IsPathIniFolderCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.IsPathIniFolderCompleted(this, new IsPathIniFolderCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/CreateLocaliFolder", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderWeb CreateLocaliFolder(string Path) {
            object[] results = this.Invoke("CreateLocaliFolder", new object[] {
                        Path});
            return ((iFolderWeb)(results[0]));
        }
        public void CreateLocaliFolderAsync(string Path) {
            this.CreateLocaliFolderAsync(Path, null);
        }
        public void CreateLocaliFolderAsync(string Path, object userState) {
            if ((this.CreateLocaliFolderOperationCompleted == null)) {
                this.CreateLocaliFolderOperationCompleted = new System.Threading.SendOrPostCallback(this.OnCreateLocaliFolderOperationCompleted);
            }
            this.InvokeAsync("CreateLocaliFolder", new object[] {
                        Path}, this.CreateLocaliFolderOperationCompleted, userState);
        }
        private void OnCreateLocaliFolderOperationCompleted(object arg) {
            if ((this.CreateLocaliFolderCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.CreateLocaliFolderCompleted(this, new CreateLocaliFolderCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/CreateiFolderInDomainEncr", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderWeb CreateiFolderInDomainEncr(string Path, string DomainID, bool SSL, string EncryptionAlgorithm, string Passphrase) {
            object[] results = this.Invoke("CreateiFolderInDomainEncr", new object[] {
                        Path,
                        DomainID,
                        SSL,
                        EncryptionAlgorithm,
                        Passphrase});
            return ((iFolderWeb)(results[0]));
        }
        public void CreateiFolderInDomainEncrAsync(string Path, string DomainID, bool SSL, string EncryptionAlgorithm, string Passphrase) {
            this.CreateiFolderInDomainEncrAsync(Path, DomainID, SSL, EncryptionAlgorithm, Passphrase, null);
        }
        public void CreateiFolderInDomainEncrAsync(string Path, string DomainID, bool SSL, string EncryptionAlgorithm, string Passphrase, object userState) {
            if ((this.CreateiFolderInDomainEncrOperationCompleted == null)) {
                this.CreateiFolderInDomainEncrOperationCompleted = new System.Threading.SendOrPostCallback(this.OnCreateiFolderInDomainEncrOperationCompleted);
            }
            this.InvokeAsync("CreateiFolderInDomainEncr", new object[] {
                        Path,
                        DomainID,
                        SSL,
                        EncryptionAlgorithm,
                        Passphrase}, this.CreateiFolderInDomainEncrOperationCompleted, userState);
        }
        private void OnCreateiFolderInDomainEncrOperationCompleted(object arg) {
            if ((this.CreateiFolderInDomainEncrCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.CreateiFolderInDomainEncrCompleted(this, new CreateiFolderInDomainEncrCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/CreateiFolderInDomain", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderWeb CreateiFolderInDomain(string Path, string DomainID) {
            object[] results = this.Invoke("CreateiFolderInDomain", new object[] {
                        Path,
                        DomainID});
            return ((iFolderWeb)(results[0]));
        }
        public void CreateiFolderInDomainAsync(string Path, string DomainID) {
            this.CreateiFolderInDomainAsync(Path, DomainID, null);
        }
        public void CreateiFolderInDomainAsync(string Path, string DomainID, object userState) {
            if ((this.CreateiFolderInDomainOperationCompleted == null)) {
                this.CreateiFolderInDomainOperationCompleted = new System.Threading.SendOrPostCallback(this.OnCreateiFolderInDomainOperationCompleted);
            }
            this.InvokeAsync("CreateiFolderInDomain", new object[] {
                        Path,
                        DomainID}, this.CreateiFolderInDomainOperationCompleted, userState);
        }
        private void OnCreateiFolderInDomainOperationCompleted(object arg) {
            if ((this.CreateiFolderInDomainCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.CreateiFolderInDomainCompleted(this, new CreateiFolderInDomainCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/CheckiFolder", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public bool CheckiFolder(string iFolderID) {
            object[] results = this.Invoke("CheckiFolder", new object[] {
                        iFolderID});
            return ((bool)(results[0]));
        }
        public void CheckiFolderAsync(string iFolderID) {
            this.CheckiFolderAsync(iFolderID, null);
        }
        public void CheckiFolderAsync(string iFolderID, object userState) {
            if ((this.CheckiFolderOperationCompleted == null)) {
                this.CheckiFolderOperationCompleted = new System.Threading.SendOrPostCallback(this.OnCheckiFolderOperationCompleted);
            }
            this.InvokeAsync("CheckiFolder", new object[] {
                        iFolderID}, this.CheckiFolderOperationCompleted, userState);
        }
        private void OnCheckiFolderOperationCompleted(object arg) {
            if ((this.CheckiFolderCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.CheckiFolderCompleted(this, new CheckiFolderCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetiFolder", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderWeb GetiFolder(string iFolderID) {
            object[] results = this.Invoke("GetiFolder", new object[] {
                        iFolderID});
            return ((iFolderWeb)(results[0]));
        }
        public void GetiFolderAsync(string iFolderID) {
            this.GetiFolderAsync(iFolderID, null);
        }
        public void GetiFolderAsync(string iFolderID, object userState) {
            if ((this.GetiFolderOperationCompleted == null)) {
                this.GetiFolderOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetiFolderOperationCompleted);
            }
            this.InvokeAsync("GetiFolder", new object[] {
                        iFolderID}, this.GetiFolderOperationCompleted, userState);
        }
        private void OnGetiFolderOperationCompleted(object arg) {
            if ((this.GetiFolderCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetiFolderCompleted(this, new GetiFolderCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetMinimaliFolder", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderWeb GetMinimaliFolder(string iFolderID, int infoToFetch) {
            object[] results = this.Invoke("GetMinimaliFolder", new object[] {
                        iFolderID,
                        infoToFetch});
            return ((iFolderWeb)(results[0]));
        }
        public void GetMinimaliFolderAsync(string iFolderID, int infoToFetch) {
            this.GetMinimaliFolderAsync(iFolderID, infoToFetch, null);
        }
        public void GetMinimaliFolderAsync(string iFolderID, int infoToFetch, object userState) {
            if ((this.GetMinimaliFolderOperationCompleted == null)) {
                this.GetMinimaliFolderOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetMinimaliFolderOperationCompleted);
            }
            this.InvokeAsync("GetMinimaliFolder", new object[] {
                        iFolderID,
                        infoToFetch}, this.GetMinimaliFolderOperationCompleted, userState);
        }
        private void OnGetMinimaliFolderOperationCompleted(object arg) {
            if ((this.GetMinimaliFolderCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetMinimaliFolderCompleted(this, new GetMinimaliFolderCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetSecurityPolicy", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public int GetSecurityPolicy(string DomainID) {
            object[] results = this.Invoke("GetSecurityPolicy", new object[] {
                        DomainID});
            return ((int)(results[0]));
        }
        public void GetSecurityPolicyAsync(string DomainID) {
            this.GetSecurityPolicyAsync(DomainID, null);
        }
        public void GetSecurityPolicyAsync(string DomainID, object userState) {
            if ((this.GetSecurityPolicyOperationCompleted == null)) {
                this.GetSecurityPolicyOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetSecurityPolicyOperationCompleted);
            }
            this.InvokeAsync("GetSecurityPolicy", new object[] {
                        DomainID}, this.GetSecurityPolicyOperationCompleted, userState);
        }
        private void OnGetSecurityPolicyOperationCompleted(object arg) {
            if ((this.GetSecurityPolicyCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetSecurityPolicyCompleted(this, new GetSecurityPolicyCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetDisableSharingPolicy", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public bool GetDisableSharingPolicy(string currentUserID, string iFolderID, string OwnerID, string DomainID) {
            object[] results = this.Invoke("GetDisableSharingPolicy", new object[] {
                        currentUserID,
                        iFolderID,
                        OwnerID,
                        DomainID});
            return ((bool)(results[0]));
        }
        public void GetDisableSharingPolicyAsync(string currentUserID, string iFolderID, string OwnerID, string DomainID) {
            this.GetDisableSharingPolicyAsync(currentUserID, iFolderID, OwnerID, DomainID, null);
        }
        public void GetDisableSharingPolicyAsync(string currentUserID, string iFolderID, string OwnerID, string DomainID, object userState) {
            if ((this.GetDisableSharingPolicyOperationCompleted == null)) {
                this.GetDisableSharingPolicyOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetDisableSharingPolicyOperationCompleted);
            }
            this.InvokeAsync("GetDisableSharingPolicy", new object[] {
                        currentUserID,
                        iFolderID,
                        OwnerID,
                        DomainID}, this.GetDisableSharingPolicyOperationCompleted, userState);
        }
        private void OnGetDisableSharingPolicyOperationCompleted(object arg) {
            if ((this.GetDisableSharingPolicyCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetDisableSharingPolicyCompleted(this, new GetDisableSharingPolicyCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/CanOwnerBeChanged", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public bool CanOwnerBeChanged(string newUserID, string domainID) {
            object[] results = this.Invoke("CanOwnerBeChanged", new object[] {
                        newUserID,
                        domainID});
            return ((bool)(results[0]));
        }
        public void CanOwnerBeChangedAsync(string newUserID, string domainID) {
            this.CanOwnerBeChangedAsync(newUserID, domainID, null);
        }
        public void CanOwnerBeChangedAsync(string newUserID, string domainID, object userState) {
            if ((this.CanOwnerBeChangedOperationCompleted == null)) {
                this.CanOwnerBeChangedOperationCompleted = new System.Threading.SendOrPostCallback(this.OnCanOwnerBeChangedOperationCompleted);
            }
            this.InvokeAsync("CanOwnerBeChanged", new object[] {
                        newUserID,
                        domainID}, this.CanOwnerBeChangedOperationCompleted, userState);
        }
        private void OnCanOwnerBeChangedOperationCompleted(object arg) {
            if ((this.CanOwnerBeChangedCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.CanOwnerBeChangedCompleted(this, new CanOwnerBeChangedCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetLimitPolicyStatus", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public int GetLimitPolicyStatus(string DomainID) {
            object[] results = this.Invoke("GetLimitPolicyStatus", new object[] {
                        DomainID});
            return ((int)(results[0]));
        }
        public void GetLimitPolicyStatusAsync(string DomainID) {
            this.GetLimitPolicyStatusAsync(DomainID, null);
        }
        public void GetLimitPolicyStatusAsync(string DomainID, object userState) {
            if ((this.GetLimitPolicyStatusOperationCompleted == null)) {
                this.GetLimitPolicyStatusOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetLimitPolicyStatusOperationCompleted);
            }
            this.InvokeAsync("GetLimitPolicyStatus", new object[] {
                        DomainID}, this.GetLimitPolicyStatusOperationCompleted, userState);
        }
        private void OnGetLimitPolicyStatusOperationCompleted(object arg) {
            if ((this.GetLimitPolicyStatusCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetLimitPolicyStatusCompleted(this, new GetLimitPolicyStatusCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetiFolderInvitation", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderWeb GetiFolderInvitation(string POBoxID, string iFolderID) {
            object[] results = this.Invoke("GetiFolderInvitation", new object[] {
                        POBoxID,
                        iFolderID});
            return ((iFolderWeb)(results[0]));
        }
        public void GetiFolderInvitationAsync(string POBoxID, string iFolderID) {
            this.GetiFolderInvitationAsync(POBoxID, iFolderID, null);
        }
        public void GetiFolderInvitationAsync(string POBoxID, string iFolderID, object userState) {
            if ((this.GetiFolderInvitationOperationCompleted == null)) {
                this.GetiFolderInvitationOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetiFolderInvitationOperationCompleted);
            }
            this.InvokeAsync("GetiFolderInvitation", new object[] {
                        POBoxID,
                        iFolderID}, this.GetiFolderInvitationOperationCompleted, userState);
        }
        private void OnGetiFolderInvitationOperationCompleted(object arg) {
            if ((this.GetiFolderInvitationCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetiFolderInvitationCompleted(this, new GetiFolderInvitationCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetiFolderByLocalPath", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderWeb GetiFolderByLocalPath(string LocalPath) {
            object[] results = this.Invoke("GetiFolderByLocalPath", new object[] {
                        LocalPath});
            return ((iFolderWeb)(results[0]));
        }
        public void GetiFolderByLocalPathAsync(string LocalPath) {
            this.GetiFolderByLocalPathAsync(LocalPath, null);
        }
        public void GetiFolderByLocalPathAsync(string LocalPath, object userState) {
            if ((this.GetiFolderByLocalPathOperationCompleted == null)) {
                this.GetiFolderByLocalPathOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetiFolderByLocalPathOperationCompleted);
            }
            this.InvokeAsync("GetiFolderByLocalPath", new object[] {
                        LocalPath}, this.GetiFolderByLocalPathOperationCompleted, userState);
        }
        private void OnGetiFolderByLocalPathOperationCompleted(object arg) {
            if ((this.GetiFolderByLocalPathCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetiFolderByLocalPathCompleted(this, new GetiFolderByLocalPathCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/DeleteiFolder", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void DeleteiFolder(string DomainID, string iFolderID) {
            this.Invoke("DeleteiFolder", new object[] {
                        DomainID,
                        iFolderID});
        }
        public void DeleteiFolderAsync(string DomainID, string iFolderID) {
            this.DeleteiFolderAsync(DomainID, iFolderID, null);
        }
        public void DeleteiFolderAsync(string DomainID, string iFolderID, object userState) {
            if ((this.DeleteiFolderOperationCompleted == null)) {
                this.DeleteiFolderOperationCompleted = new System.Threading.SendOrPostCallback(this.OnDeleteiFolderOperationCompleted);
            }
            this.InvokeAsync("DeleteiFolder", new object[] {
                        DomainID,
                        iFolderID}, this.DeleteiFolderOperationCompleted, userState);
        }
        private void OnDeleteiFolderOperationCompleted(object arg) {
            if ((this.DeleteiFolderCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.DeleteiFolderCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/RevertiFolder", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderWeb RevertiFolder(string iFolderID) {
            object[] results = this.Invoke("RevertiFolder", new object[] {
                        iFolderID});
            return ((iFolderWeb)(results[0]));
        }
        public void RevertiFolderAsync(string iFolderID) {
            this.RevertiFolderAsync(iFolderID, null);
        }
        public void RevertiFolderAsync(string iFolderID, object userState) {
            if ((this.RevertiFolderOperationCompleted == null)) {
                this.RevertiFolderOperationCompleted = new System.Threading.SendOrPostCallback(this.OnRevertiFolderOperationCompleted);
            }
            this.InvokeAsync("RevertiFolder", new object[] {
                        iFolderID}, this.RevertiFolderOperationCompleted, userState);
        }
        private void OnRevertiFolderOperationCompleted(object arg) {
            if ((this.RevertiFolderCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.RevertiFolderCompleted(this, new RevertiFolderCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/RevertiFolder1", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderWeb RevertiFolder1(string iFolderID) {
            object[] results = this.Invoke("RevertiFolder1", new object[] {
                        iFolderID});
            return ((iFolderWeb)(results[0]));
        }
        public void RevertiFolder1Async(string iFolderID) {
            this.RevertiFolder1Async(iFolderID, null);
        }
        public void RevertiFolder1Async(string iFolderID, object userState) {
            if ((this.RevertiFolder1OperationCompleted == null)) {
                this.RevertiFolder1OperationCompleted = new System.Threading.SendOrPostCallback(this.OnRevertiFolder1OperationCompleted);
            }
            this.InvokeAsync("RevertiFolder1", new object[] {
                        iFolderID}, this.RevertiFolder1OperationCompleted, userState);
        }
        private void OnRevertiFolder1OperationCompleted(object arg) {
            if ((this.RevertiFolder1Completed != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.RevertiFolder1Completed(this, new RevertiFolder1CompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetAlliFolders", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderWeb[] GetAlliFolders() {
            object[] results = this.Invoke("GetAlliFolders", new object[0]);
            return ((iFolderWeb[])(results[0]));
        }
        public void GetAlliFoldersAsync() {
            this.GetAlliFoldersAsync(null);
        }
        public void GetAlliFoldersAsync(object userState) {
            if ((this.GetAlliFoldersOperationCompleted == null)) {
                this.GetAlliFoldersOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetAlliFoldersOperationCompleted);
            }
            this.InvokeAsync("GetAlliFolders", new object[0], this.GetAlliFoldersOperationCompleted, userState);
        }
        private void OnGetAlliFoldersOperationCompleted(object arg) {
            if ((this.GetAlliFoldersCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetAlliFoldersCompleted(this, new GetAlliFoldersCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetAlliFolders1", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderWeb[] GetAlliFolders1() {
            object[] results = this.Invoke("GetAlliFolders1", new object[0]);
            return ((iFolderWeb[])(results[0]));
        }
        public void GetAlliFolders1Async() {
            this.GetAlliFolders1Async(null);
        }
        public void GetAlliFolders1Async(object userState) {
            if ((this.GetAlliFolders1OperationCompleted == null)) {
                this.GetAlliFolders1OperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetAlliFolders1OperationCompleted);
            }
            this.InvokeAsync("GetAlliFolders1", new object[0], this.GetAlliFolders1OperationCompleted, userState);
        }
        private void OnGetAlliFolders1OperationCompleted(object arg) {
            if ((this.GetAlliFolders1Completed != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetAlliFolders1Completed(this, new GetAlliFolders1CompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetiFoldersForDomain", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderWeb[] GetiFoldersForDomain(string DomainID) {
            object[] results = this.Invoke("GetiFoldersForDomain", new object[] {
                        DomainID});
            return ((iFolderWeb[])(results[0]));
        }
        public void GetiFoldersForDomainAsync(string DomainID) {
            this.GetiFoldersForDomainAsync(DomainID, null);
        }
        public void GetiFoldersForDomainAsync(string DomainID, object userState) {
            if ((this.GetiFoldersForDomainOperationCompleted == null)) {
                this.GetiFoldersForDomainOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetiFoldersForDomainOperationCompleted);
            }
            this.InvokeAsync("GetiFoldersForDomain", new object[] {
                        DomainID}, this.GetiFoldersForDomainOperationCompleted, userState);
        }
        private void OnGetiFoldersForDomainOperationCompleted(object arg) {
            if ((this.GetiFoldersForDomainCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetiFoldersForDomainCompleted(this, new GetiFoldersForDomainCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetiFolders", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderWeb[] GetiFolders(string UserID) {
            object[] results = this.Invoke("GetiFolders", new object[] {
                        UserID});
            return ((iFolderWeb[])(results[0]));
        }
        public void GetiFoldersAsync(string UserID) {
            this.GetiFoldersAsync(UserID, null);
        }
        public void GetiFoldersAsync(string UserID, object userState) {
            if ((this.GetiFoldersOperationCompleted == null)) {
                this.GetiFoldersOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetiFoldersOperationCompleted);
            }
            this.InvokeAsync("GetiFolders", new object[] {
                        UserID}, this.GetiFoldersOperationCompleted, userState);
        }
        private void OnGetiFoldersOperationCompleted(object arg) {
            if ((this.GetiFoldersCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetiFoldersCompleted(this, new GetiFoldersCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/SetUserRights", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void SetUserRights(string iFolderID, string UserID, string Rights) {
            this.Invoke("SetUserRights", new object[] {
                        iFolderID,
                        UserID,
                        Rights});
        }
        public void SetUserRightsAsync(string iFolderID, string UserID, string Rights) {
            this.SetUserRightsAsync(iFolderID, UserID, Rights, null);
        }
        public void SetUserRightsAsync(string iFolderID, string UserID, string Rights, object userState) {
            if ((this.SetUserRightsOperationCompleted == null)) {
                this.SetUserRightsOperationCompleted = new System.Threading.SendOrPostCallback(this.OnSetUserRightsOperationCompleted);
            }
            this.InvokeAsync("SetUserRights", new object[] {
                        iFolderID,
                        UserID,
                        Rights}, this.SetUserRightsOperationCompleted, userState);
        }
        private void OnSetUserRightsOperationCompleted(object arg) {
            if ((this.SetUserRightsCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.SetUserRightsCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetOwner", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderUser GetOwner(string iFolderID) {
            object[] results = this.Invoke("GetOwner", new object[] {
                        iFolderID});
            return ((iFolderUser)(results[0]));
        }
        public void GetOwnerAsync(string iFolderID) {
            this.GetOwnerAsync(iFolderID, null);
        }
        public void GetOwnerAsync(string iFolderID, object userState) {
            if ((this.GetOwnerOperationCompleted == null)) {
                this.GetOwnerOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetOwnerOperationCompleted);
            }
            this.InvokeAsync("GetOwner", new object[] {
                        iFolderID}, this.GetOwnerOperationCompleted, userState);
        }
        private void OnGetOwnerOperationCompleted(object arg) {
            if ((this.GetOwnerCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetOwnerCompleted(this, new GetOwnerCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/ChangeOwner", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void ChangeOwner(string iFolderID, string NewOwnerUserID, string OldOwnerRights) {
            this.Invoke("ChangeOwner", new object[] {
                        iFolderID,
                        NewOwnerUserID,
                        OldOwnerRights});
        }
        public void ChangeOwnerAsync(string iFolderID, string NewOwnerUserID, string OldOwnerRights) {
            this.ChangeOwnerAsync(iFolderID, NewOwnerUserID, OldOwnerRights, null);
        }
        public void ChangeOwnerAsync(string iFolderID, string NewOwnerUserID, string OldOwnerRights, object userState) {
            if ((this.ChangeOwnerOperationCompleted == null)) {
                this.ChangeOwnerOperationCompleted = new System.Threading.SendOrPostCallback(this.OnChangeOwnerOperationCompleted);
            }
            this.InvokeAsync("ChangeOwner", new object[] {
                        iFolderID,
                        NewOwnerUserID,
                        OldOwnerRights}, this.ChangeOwnerOperationCompleted, userState);
        }
        private void OnChangeOwnerOperationCompleted(object arg) {
            if ((this.ChangeOwnerCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.ChangeOwnerCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/RemoveiFolderUser", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void RemoveiFolderUser(string iFolderID, string UserID) {
            this.Invoke("RemoveiFolderUser", new object[] {
                        iFolderID,
                        UserID});
        }
        public void RemoveiFolderUserAsync(string iFolderID, string UserID) {
            this.RemoveiFolderUserAsync(iFolderID, UserID, null);
        }
        public void RemoveiFolderUserAsync(string iFolderID, string UserID, object userState) {
            if ((this.RemoveiFolderUserOperationCompleted == null)) {
                this.RemoveiFolderUserOperationCompleted = new System.Threading.SendOrPostCallback(this.OnRemoveiFolderUserOperationCompleted);
            }
            this.InvokeAsync("RemoveiFolderUser", new object[] {
                        iFolderID,
                        UserID}, this.RemoveiFolderUserOperationCompleted, userState);
        }
        private void OnRemoveiFolderUserOperationCompleted(object arg) {
            if ((this.RemoveiFolderUserCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.RemoveiFolderUserCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetiFolderUsers", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderUser[] GetiFolderUsers(string iFolderID) {
            object[] results = this.Invoke("GetiFolderUsers", new object[] {
                        iFolderID});
            return ((iFolderUser[])(results[0]));
        }
        public void GetiFolderUsersAsync(string iFolderID) {
            this.GetiFolderUsersAsync(iFolderID, null);
        }
        public void GetiFolderUsersAsync(string iFolderID, object userState) {
            if ((this.GetiFolderUsersOperationCompleted == null)) {
                this.GetiFolderUsersOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetiFolderUsersOperationCompleted);
            }
            this.InvokeAsync("GetiFolderUsers", new object[] {
                        iFolderID}, this.GetiFolderUsersOperationCompleted, userState);
        }
        private void OnGetiFolderUsersOperationCompleted(object arg) {
            if ((this.GetiFolderUsersCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetiFolderUsersCompleted(this, new GetiFolderUsersCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetDomainUsers", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderUser[] GetDomainUsers(string DomainID, int numUsers) {
            object[] results = this.Invoke("GetDomainUsers", new object[] {
                        DomainID,
                        numUsers});
            return ((iFolderUser[])(results[0]));
        }
        public void GetDomainUsersAsync(string DomainID, int numUsers) {
            this.GetDomainUsersAsync(DomainID, numUsers, null);
        }
        public void GetDomainUsersAsync(string DomainID, int numUsers, object userState) {
            if ((this.GetDomainUsersOperationCompleted == null)) {
                this.GetDomainUsersOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetDomainUsersOperationCompleted);
            }
            this.InvokeAsync("GetDomainUsers", new object[] {
                        DomainID,
                        numUsers}, this.GetDomainUsersOperationCompleted, userState);
        }
        private void OnGetDomainUsersOperationCompleted(object arg) {
            if ((this.GetDomainUsersCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetDomainUsersCompleted(this, new GetDomainUsersCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/SearchForDomainUsers", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderUser[] SearchForDomainUsers(string DomainID, string SearchString) {
            object[] results = this.Invoke("SearchForDomainUsers", new object[] {
                        DomainID,
                        SearchString});
            return ((iFolderUser[])(results[0]));
        }
        public void SearchForDomainUsersAsync(string DomainID, string SearchString) {
            this.SearchForDomainUsersAsync(DomainID, SearchString, null);
        }
        public void SearchForDomainUsersAsync(string DomainID, string SearchString, object userState) {
            if ((this.SearchForDomainUsersOperationCompleted == null)) {
                this.SearchForDomainUsersOperationCompleted = new System.Threading.SendOrPostCallback(this.OnSearchForDomainUsersOperationCompleted);
            }
            this.InvokeAsync("SearchForDomainUsers", new object[] {
                        DomainID,
                        SearchString}, this.SearchForDomainUsersOperationCompleted, userState);
        }
        private void OnSearchForDomainUsersOperationCompleted(object arg) {
            if ((this.SearchForDomainUsersCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.SearchForDomainUsersCompleted(this, new SearchForDomainUsersCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/FindCloseiFolderMembers", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void FindCloseiFolderMembers(string domainID, string searchContext) {
            this.Invoke("FindCloseiFolderMembers", new object[] {
                        domainID,
                        searchContext});
        }
        public void FindCloseiFolderMembersAsync(string domainID, string searchContext) {
            this.FindCloseiFolderMembersAsync(domainID, searchContext, null);
        }
        public void FindCloseiFolderMembersAsync(string domainID, string searchContext, object userState) {
            if ((this.FindCloseiFolderMembersOperationCompleted == null)) {
                this.FindCloseiFolderMembersOperationCompleted = new System.Threading.SendOrPostCallback(this.OnFindCloseiFolderMembersOperationCompleted);
            }
            this.InvokeAsync("FindCloseiFolderMembers", new object[] {
                        domainID,
                        searchContext}, this.FindCloseiFolderMembersOperationCompleted, userState);
        }
        private void OnFindCloseiFolderMembersOperationCompleted(object arg) {
            if ((this.FindCloseiFolderMembersCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.FindCloseiFolderMembersCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/FindFirstiFolderMembers", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public bool FindFirstiFolderMembers(string domainID, int count, out string searchContext, out iFolderUser[] memberList, out int totalMembers) {
            object[] results = this.Invoke("FindFirstiFolderMembers", new object[] {
                        domainID,
                        count});
            searchContext = ((string)(results[1]));
            memberList = ((iFolderUser[])(results[2]));
            totalMembers = ((int)(results[3]));
            return ((bool)(results[0]));
        }
        public void FindFirstiFolderMembersAsync(string domainID, int count) {
            this.FindFirstiFolderMembersAsync(domainID, count, null);
        }
        public void FindFirstiFolderMembersAsync(string domainID, int count, object userState) {
            if ((this.FindFirstiFolderMembersOperationCompleted == null)) {
                this.FindFirstiFolderMembersOperationCompleted = new System.Threading.SendOrPostCallback(this.OnFindFirstiFolderMembersOperationCompleted);
            }
            this.InvokeAsync("FindFirstiFolderMembers", new object[] {
                        domainID,
                        count}, this.FindFirstiFolderMembersOperationCompleted, userState);
        }
        private void OnFindFirstiFolderMembersOperationCompleted(object arg) {
            if ((this.FindFirstiFolderMembersCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.FindFirstiFolderMembersCompleted(this, new FindFirstiFolderMembersCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/FindFirstSpecificiFolderMembers", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public bool FindFirstSpecificiFolderMembers(string domainID, string attributeName, string searchString, iFolderSearchType operation, int count, out string searchContext, out iFolderUser[] memberList, out int totalMembers) {
            object[] results = this.Invoke("FindFirstSpecificiFolderMembers", new object[] {
                        domainID,
                        attributeName,
                        searchString,
                        operation,
                        count});
            searchContext = ((string)(results[1]));
            memberList = ((iFolderUser[])(results[2]));
            totalMembers = ((int)(results[3]));
            return ((bool)(results[0]));
        }
        public void FindFirstSpecificiFolderMembersAsync(string domainID, string attributeName, string searchString, iFolderSearchType operation, int count) {
            this.FindFirstSpecificiFolderMembersAsync(domainID, attributeName, searchString, operation, count, null);
        }
        public void FindFirstSpecificiFolderMembersAsync(string domainID, string attributeName, string searchString, iFolderSearchType operation, int count, object userState) {
            if ((this.FindFirstSpecificiFolderMembersOperationCompleted == null)) {
                this.FindFirstSpecificiFolderMembersOperationCompleted = new System.Threading.SendOrPostCallback(this.OnFindFirstSpecificiFolderMembersOperationCompleted);
            }
            this.InvokeAsync("FindFirstSpecificiFolderMembers", new object[] {
                        domainID,
                        attributeName,
                        searchString,
                        operation,
                        count}, this.FindFirstSpecificiFolderMembersOperationCompleted, userState);
        }
        private void OnFindFirstSpecificiFolderMembersOperationCompleted(object arg) {
            if ((this.FindFirstSpecificiFolderMembersCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.FindFirstSpecificiFolderMembersCompleted(this, new FindFirstSpecificiFolderMembersCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/FindNextiFolderMembers", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public bool FindNextiFolderMembers(string domainID, ref string searchContext, int count, out iFolderUser[] memberList) {
            object[] results = this.Invoke("FindNextiFolderMembers", new object[] {
                        domainID,
                        searchContext,
                        count});
            searchContext = ((string)(results[1]));
            memberList = ((iFolderUser[])(results[2]));
            return ((bool)(results[0]));
        }
        public void FindNextiFolderMembersAsync(string domainID, string searchContext, int count) {
            this.FindNextiFolderMembersAsync(domainID, searchContext, count, null);
        }
        public void FindNextiFolderMembersAsync(string domainID, string searchContext, int count, object userState) {
            if ((this.FindNextiFolderMembersOperationCompleted == null)) {
                this.FindNextiFolderMembersOperationCompleted = new System.Threading.SendOrPostCallback(this.OnFindNextiFolderMembersOperationCompleted);
            }
            this.InvokeAsync("FindNextiFolderMembers", new object[] {
                        domainID,
                        searchContext,
                        count}, this.FindNextiFolderMembersOperationCompleted, userState);
        }
        private void OnFindNextiFolderMembersOperationCompleted(object arg) {
            if ((this.FindNextiFolderMembersCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.FindNextiFolderMembersCompleted(this, new FindNextiFolderMembersCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/FindPreviousiFolderMembers", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public bool FindPreviousiFolderMembers(string domainID, ref string searchContext, int count, out iFolderUser[] memberList) {
            object[] results = this.Invoke("FindPreviousiFolderMembers", new object[] {
                        domainID,
                        searchContext,
                        count});
            searchContext = ((string)(results[1]));
            memberList = ((iFolderUser[])(results[2]));
            return ((bool)(results[0]));
        }
        public void FindPreviousiFolderMembersAsync(string domainID, string searchContext, int count) {
            this.FindPreviousiFolderMembersAsync(domainID, searchContext, count, null);
        }
        public void FindPreviousiFolderMembersAsync(string domainID, string searchContext, int count, object userState) {
            if ((this.FindPreviousiFolderMembersOperationCompleted == null)) {
                this.FindPreviousiFolderMembersOperationCompleted = new System.Threading.SendOrPostCallback(this.OnFindPreviousiFolderMembersOperationCompleted);
            }
            this.InvokeAsync("FindPreviousiFolderMembers", new object[] {
                        domainID,
                        searchContext,
                        count}, this.FindPreviousiFolderMembersOperationCompleted, userState);
        }
        private void OnFindPreviousiFolderMembersOperationCompleted(object arg) {
            if ((this.FindPreviousiFolderMembersCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.FindPreviousiFolderMembersCompleted(this, new FindPreviousiFolderMembersCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/FindSeekiFolderMembers", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public bool FindSeekiFolderMembers(string domainID, ref string searchContext, int offset, int count, out iFolderUser[] memberList) {
            object[] results = this.Invoke("FindSeekiFolderMembers", new object[] {
                        domainID,
                        searchContext,
                        offset,
                        count});
            searchContext = ((string)(results[1]));
            memberList = ((iFolderUser[])(results[2]));
            return ((bool)(results[0]));
        }
        public void FindSeekiFolderMembersAsync(string domainID, string searchContext, int offset, int count) {
            this.FindSeekiFolderMembersAsync(domainID, searchContext, offset, count, null);
        }
        public void FindSeekiFolderMembersAsync(string domainID, string searchContext, int offset, int count, object userState) {
            if ((this.FindSeekiFolderMembersOperationCompleted == null)) {
                this.FindSeekiFolderMembersOperationCompleted = new System.Threading.SendOrPostCallback(this.OnFindSeekiFolderMembersOperationCompleted);
            }
            this.InvokeAsync("FindSeekiFolderMembers", new object[] {
                        domainID,
                        searchContext,
                        offset,
                        count}, this.FindSeekiFolderMembersOperationCompleted, userState);
        }
        private void OnFindSeekiFolderMembersOperationCompleted(object arg) {
            if ((this.FindSeekiFolderMembersCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.FindSeekiFolderMembersCompleted(this, new FindSeekiFolderMembersCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetiFolderUser", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderUser GetiFolderUser(string UserID) {
            object[] results = this.Invoke("GetiFolderUser", new object[] {
                        UserID});
            return ((iFolderUser)(results[0]));
        }
        public void GetiFolderUserAsync(string UserID) {
            this.GetiFolderUserAsync(UserID, null);
        }
        public void GetiFolderUserAsync(string UserID, object userState) {
            if ((this.GetiFolderUserOperationCompleted == null)) {
                this.GetiFolderUserOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetiFolderUserOperationCompleted);
            }
            this.InvokeAsync("GetiFolderUser", new object[] {
                        UserID}, this.GetiFolderUserOperationCompleted, userState);
        }
        private void OnGetiFolderUserOperationCompleted(object arg) {
            if ((this.GetiFolderUserCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetiFolderUserCompleted(this, new GetiFolderUserCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetRAName", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public string GetRAName(string DomainID) {
            object[] results = this.Invoke("GetRAName", new object[] {
                        DomainID});
            return ((string)(results[0]));
        }
        public void GetRANameAsync(string DomainID) {
            this.GetRANameAsync(DomainID, null);
        }
        public void GetRANameAsync(string DomainID, object userState) {
            if ((this.GetRANameOperationCompleted == null)) {
                this.GetRANameOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetRANameOperationCompleted);
            }
            this.InvokeAsync("GetRAName", new object[] {
                        DomainID}, this.GetRANameOperationCompleted, userState);
        }
        private void OnGetRANameOperationCompleted(object arg) {
            if ((this.GetRANameCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetRANameCompleted(this, new GetRANameCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetiFolderUserFromNodeID", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderUser GetiFolderUserFromNodeID(string CollectionID, string NodeID) {
            object[] results = this.Invoke("GetiFolderUserFromNodeID", new object[] {
                        CollectionID,
                        NodeID});
            return ((iFolderUser)(results[0]));
        }
        public void GetiFolderUserFromNodeIDAsync(string CollectionID, string NodeID) {
            this.GetiFolderUserFromNodeIDAsync(CollectionID, NodeID, null);
        }
        public void GetiFolderUserFromNodeIDAsync(string CollectionID, string NodeID, object userState) {
            if ((this.GetiFolderUserFromNodeIDOperationCompleted == null)) {
                this.GetiFolderUserFromNodeIDOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetiFolderUserFromNodeIDOperationCompleted);
            }
            this.InvokeAsync("GetiFolderUserFromNodeID", new object[] {
                        CollectionID,
                        NodeID}, this.GetiFolderUserFromNodeIDOperationCompleted, userState);
        }
        private void OnGetiFolderUserFromNodeIDOperationCompleted(object arg) {
            if ((this.GetiFolderUserFromNodeIDCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetiFolderUserFromNodeIDCompleted(this, new GetiFolderUserFromNodeIDCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/AddAndInviteUser", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderUser AddAndInviteUser(string iFolderID, string MemberName, string GivenName, string FamilyName, string MemberID, string PublicKey, string Rights) {
            object[] results = this.Invoke("AddAndInviteUser", new object[] {
                        iFolderID,
                        MemberName,
                        GivenName,
                        FamilyName,
                        MemberID,
                        PublicKey,
                        Rights});
            return ((iFolderUser)(results[0]));
        }
        public void AddAndInviteUserAsync(string iFolderID, string MemberName, string GivenName, string FamilyName, string MemberID, string PublicKey, string Rights) {
            this.AddAndInviteUserAsync(iFolderID, MemberName, GivenName, FamilyName, MemberID, PublicKey, Rights, null);
        }
        public void AddAndInviteUserAsync(string iFolderID, string MemberName, string GivenName, string FamilyName, string MemberID, string PublicKey, string Rights, object userState) {
            if ((this.AddAndInviteUserOperationCompleted == null)) {
                this.AddAndInviteUserOperationCompleted = new System.Threading.SendOrPostCallback(this.OnAddAndInviteUserOperationCompleted);
            }
            this.InvokeAsync("AddAndInviteUser", new object[] {
                        iFolderID,
                        MemberName,
                        GivenName,
                        FamilyName,
                        MemberID,
                        PublicKey,
                        Rights}, this.AddAndInviteUserOperationCompleted, userState);
        }
        private void OnAddAndInviteUserOperationCompleted(object arg) {
            if ((this.AddAndInviteUserCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.AddAndInviteUserCompleted(this, new AddAndInviteUserCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/InviteUser", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderUser InviteUser(string iFolderID, string UserID, string Rights) {
            object[] results = this.Invoke("InviteUser", new object[] {
                        iFolderID,
                        UserID,
                        Rights});
            return ((iFolderUser)(results[0]));
        }
        public void InviteUserAsync(string iFolderID, string UserID, string Rights) {
            this.InviteUserAsync(iFolderID, UserID, Rights, null);
        }
        public void InviteUserAsync(string iFolderID, string UserID, string Rights, object userState) {
            if ((this.InviteUserOperationCompleted == null)) {
                this.InviteUserOperationCompleted = new System.Threading.SendOrPostCallback(this.OnInviteUserOperationCompleted);
            }
            this.InvokeAsync("InviteUser", new object[] {
                        iFolderID,
                        UserID,
                        Rights}, this.InviteUserOperationCompleted, userState);
        }
        private void OnInviteUserOperationCompleted(object arg) {
            if ((this.InviteUserCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.InviteUserCompleted(this, new InviteUserCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/MergeiFolder", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderWeb MergeiFolder(string DomainID, string iFolderID, string path) {
            object[] results = this.Invoke("MergeiFolder", new object[] {
                        DomainID,
                        iFolderID,
                        path});
            return ((iFolderWeb)(results[0]));
        }
        public void MergeiFolderAsync(string DomainID, string iFolderID, string path) {
            this.MergeiFolderAsync(DomainID, iFolderID, path, null);
        }
        public void MergeiFolderAsync(string DomainID, string iFolderID, string path, object userState) {
            if ((this.MergeiFolderOperationCompleted == null)) {
                this.MergeiFolderOperationCompleted = new System.Threading.SendOrPostCallback(this.OnMergeiFolderOperationCompleted);
            }
            this.InvokeAsync("MergeiFolder", new object[] {
                        DomainID,
                        iFolderID,
                        path}, this.MergeiFolderOperationCompleted, userState);
        }
        private void OnMergeiFolderOperationCompleted(object arg) {
            if ((this.MergeiFolderCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.MergeiFolderCompleted(this, new MergeiFolderCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/AcceptiFolderInvitation", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderWeb AcceptiFolderInvitation(string DomainID, string iFolderID, string LocalPath) {
            object[] results = this.Invoke("AcceptiFolderInvitation", new object[] {
                        DomainID,
                        iFolderID,
                        LocalPath});
            return ((iFolderWeb)(results[0]));
        }
        public void AcceptiFolderInvitationAsync(string DomainID, string iFolderID, string LocalPath) {
            this.AcceptiFolderInvitationAsync(DomainID, iFolderID, LocalPath, null);
        }
        public void AcceptiFolderInvitationAsync(string DomainID, string iFolderID, string LocalPath, object userState) {
            if ((this.AcceptiFolderInvitationOperationCompleted == null)) {
                this.AcceptiFolderInvitationOperationCompleted = new System.Threading.SendOrPostCallback(this.OnAcceptiFolderInvitationOperationCompleted);
            }
            this.InvokeAsync("AcceptiFolderInvitation", new object[] {
                        DomainID,
                        iFolderID,
                        LocalPath}, this.AcceptiFolderInvitationOperationCompleted, userState);
        }
        private void OnAcceptiFolderInvitationOperationCompleted(object arg) {
            if ((this.AcceptiFolderInvitationCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.AcceptiFolderInvitationCompleted(this, new AcceptiFolderInvitationCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/AcceptiFolderInvitation1", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public iFolderWeb AcceptiFolderInvitation1(string DomainID, string iFolderID, string LocalPath) {
            object[] results = this.Invoke("AcceptiFolderInvitation1", new object[] {
                        DomainID,
                        iFolderID,
                        LocalPath});
            return ((iFolderWeb)(results[0]));
        }
        public void AcceptiFolderInvitation1Async(string DomainID, string iFolderID, string LocalPath) {
            this.AcceptiFolderInvitation1Async(DomainID, iFolderID, LocalPath, null);
        }
        public void AcceptiFolderInvitation1Async(string DomainID, string iFolderID, string LocalPath, object userState) {
            if ((this.AcceptiFolderInvitation1OperationCompleted == null)) {
                this.AcceptiFolderInvitation1OperationCompleted = new System.Threading.SendOrPostCallback(this.OnAcceptiFolderInvitation1OperationCompleted);
            }
            this.InvokeAsync("AcceptiFolderInvitation1", new object[] {
                        DomainID,
                        iFolderID,
                        LocalPath}, this.AcceptiFolderInvitation1OperationCompleted, userState);
        }
        private void OnAcceptiFolderInvitation1OperationCompleted(object arg) {
            if ((this.AcceptiFolderInvitation1Completed != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.AcceptiFolderInvitation1Completed(this, new AcceptiFolderInvitation1CompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/CheckForMacUpdate", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public int CheckForMacUpdate(string DomainID, string CurrentVersion, out string ServerVersion) {
            object[] results = this.Invoke("CheckForMacUpdate", new object[] {
                        DomainID,
                        CurrentVersion});
            ServerVersion = ((string)(results[1]));
            return ((int)(results[0]));
        }
        public void CheckForMacUpdateAsync(string DomainID, string CurrentVersion) {
            this.CheckForMacUpdateAsync(DomainID, CurrentVersion, null);
        }
        public void CheckForMacUpdateAsync(string DomainID, string CurrentVersion, object userState) {
            if ((this.CheckForMacUpdateOperationCompleted == null)) {
                this.CheckForMacUpdateOperationCompleted = new System.Threading.SendOrPostCallback(this.OnCheckForMacUpdateOperationCompleted);
            }
            this.InvokeAsync("CheckForMacUpdate", new object[] {
                        DomainID,
                        CurrentVersion}, this.CheckForMacUpdateOperationCompleted, userState);
        }
        private void OnCheckForMacUpdateOperationCompleted(object arg) {
            if ((this.CheckForMacUpdateCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.CheckForMacUpdateCompleted(this, new CheckForMacUpdateCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/DeclineiFolderInvitation", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void DeclineiFolderInvitation(string DomainID, string iFolderID) {
            this.Invoke("DeclineiFolderInvitation", new object[] {
                        DomainID,
                        iFolderID});
        }
        public void DeclineiFolderInvitationAsync(string DomainID, string iFolderID) {
            this.DeclineiFolderInvitationAsync(DomainID, iFolderID, null);
        }
        public void DeclineiFolderInvitationAsync(string DomainID, string iFolderID, object userState) {
            if ((this.DeclineiFolderInvitationOperationCompleted == null)) {
                this.DeclineiFolderInvitationOperationCompleted = new System.Threading.SendOrPostCallback(this.OnDeclineiFolderInvitationOperationCompleted);
            }
            this.InvokeAsync("DeclineiFolderInvitation", new object[] {
                        DomainID,
                        iFolderID}, this.DeclineiFolderInvitationOperationCompleted, userState);
        }
        private void OnDeclineiFolderInvitationOperationCompleted(object arg) {
            if ((this.DeclineiFolderInvitationCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.DeclineiFolderInvitationCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetUserDiskSpace", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public DiskSpace GetUserDiskSpace(string UserID) {
            object[] results = this.Invoke("GetUserDiskSpace", new object[] {
                        UserID});
            return ((DiskSpace)(results[0]));
        }
        public void GetUserDiskSpaceAsync(string UserID) {
            this.GetUserDiskSpaceAsync(UserID, null);
        }
        public void GetUserDiskSpaceAsync(string UserID, object userState) {
            if ((this.GetUserDiskSpaceOperationCompleted == null)) {
                this.GetUserDiskSpaceOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetUserDiskSpaceOperationCompleted);
            }
            this.InvokeAsync("GetUserDiskSpace", new object[] {
                        UserID}, this.GetUserDiskSpaceOperationCompleted, userState);
        }
        private void OnGetUserDiskSpaceOperationCompleted(object arg) {
            if ((this.GetUserDiskSpaceCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetUserDiskSpaceCompleted(this, new GetUserDiskSpaceCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetiFolderDiskSpace", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public DiskSpace GetiFolderDiskSpace(string iFolderID) {
            object[] results = this.Invoke("GetiFolderDiskSpace", new object[] {
                        iFolderID});
            return ((DiskSpace)(results[0]));
        }
        public void GetiFolderDiskSpaceAsync(string iFolderID) {
            this.GetiFolderDiskSpaceAsync(iFolderID, null);
        }
        public void GetiFolderDiskSpaceAsync(string iFolderID, object userState) {
            if ((this.GetiFolderDiskSpaceOperationCompleted == null)) {
                this.GetiFolderDiskSpaceOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetiFolderDiskSpaceOperationCompleted);
            }
            this.InvokeAsync("GetiFolderDiskSpace", new object[] {
                        iFolderID}, this.GetiFolderDiskSpaceOperationCompleted, userState);
        }
        private void OnGetiFolderDiskSpaceOperationCompleted(object arg) {
            if ((this.GetiFolderDiskSpaceCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetiFolderDiskSpaceCompleted(this, new GetiFolderDiskSpaceCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/SetUserDiskSpaceLimit", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void SetUserDiskSpaceLimit(string UserID, long Limit) {
            this.Invoke("SetUserDiskSpaceLimit", new object[] {
                        UserID,
                        Limit});
        }
        public void SetUserDiskSpaceLimitAsync(string UserID, long Limit) {
            this.SetUserDiskSpaceLimitAsync(UserID, Limit, null);
        }
        public void SetUserDiskSpaceLimitAsync(string UserID, long Limit, object userState) {
            if ((this.SetUserDiskSpaceLimitOperationCompleted == null)) {
                this.SetUserDiskSpaceLimitOperationCompleted = new System.Threading.SendOrPostCallback(this.OnSetUserDiskSpaceLimitOperationCompleted);
            }
            this.InvokeAsync("SetUserDiskSpaceLimit", new object[] {
                        UserID,
                        Limit}, this.SetUserDiskSpaceLimitOperationCompleted, userState);
        }
        private void OnSetUserDiskSpaceLimitOperationCompleted(object arg) {
            if ((this.SetUserDiskSpaceLimitCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.SetUserDiskSpaceLimitCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/SetiFolderDiskSpaceLimit", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void SetiFolderDiskSpaceLimit(string iFolderID, long Limit) {
            this.Invoke("SetiFolderDiskSpaceLimit", new object[] {
                        iFolderID,
                        Limit});
        }
        public void SetiFolderDiskSpaceLimitAsync(string iFolderID, long Limit) {
            this.SetiFolderDiskSpaceLimitAsync(iFolderID, Limit, null);
        }
        public void SetiFolderDiskSpaceLimitAsync(string iFolderID, long Limit, object userState) {
            if ((this.SetiFolderDiskSpaceLimitOperationCompleted == null)) {
                this.SetiFolderDiskSpaceLimitOperationCompleted = new System.Threading.SendOrPostCallback(this.OnSetiFolderDiskSpaceLimitOperationCompleted);
            }
            this.InvokeAsync("SetiFolderDiskSpaceLimit", new object[] {
                        iFolderID,
                        Limit}, this.SetiFolderDiskSpaceLimitOperationCompleted, userState);
        }
        private void OnSetiFolderDiskSpaceLimitOperationCompleted(object arg) {
            if ((this.SetiFolderDiskSpaceLimitCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.SetiFolderDiskSpaceLimitCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/SetiFolderSecureSync", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void SetiFolderSecureSync(string iFolderID, bool ssl) {
            this.Invoke("SetiFolderSecureSync", new object[] {
                        iFolderID,
                        ssl});
        }
        public void SetiFolderSecureSyncAsync(string iFolderID, bool ssl) {
            this.SetiFolderSecureSyncAsync(iFolderID, ssl, null);
        }
        public void SetiFolderSecureSyncAsync(string iFolderID, bool ssl, object userState) {
            if ((this.SetiFolderSecureSyncOperationCompleted == null)) {
                this.SetiFolderSecureSyncOperationCompleted = new System.Threading.SendOrPostCallback(this.OnSetiFolderSecureSyncOperationCompleted);
            }
            this.InvokeAsync("SetiFolderSecureSync", new object[] {
                        iFolderID,
                        ssl}, this.SetiFolderSecureSyncOperationCompleted, userState);
        }
        private void OnSetiFolderSecureSyncOperationCompleted(object arg) {
            if ((this.SetiFolderSecureSyncCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.SetiFolderSecureSyncCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/SetiFolderSyncInterval", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void SetiFolderSyncInterval(string iFolderID, int Interval) {
            this.Invoke("SetiFolderSyncInterval", new object[] {
                        iFolderID,
                        Interval});
        }
        public void SetiFolderSyncIntervalAsync(string iFolderID, int Interval) {
            this.SetiFolderSyncIntervalAsync(iFolderID, Interval, null);
        }
        public void SetiFolderSyncIntervalAsync(string iFolderID, int Interval, object userState) {
            if ((this.SetiFolderSyncIntervalOperationCompleted == null)) {
                this.SetiFolderSyncIntervalOperationCompleted = new System.Threading.SendOrPostCallback(this.OnSetiFolderSyncIntervalOperationCompleted);
            }
            this.InvokeAsync("SetiFolderSyncInterval", new object[] {
                        iFolderID,
                        Interval}, this.SetiFolderSyncIntervalOperationCompleted, userState);
        }
        private void OnSetiFolderSyncIntervalOperationCompleted(object arg) {
            if ((this.SetiFolderSyncIntervalCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.SetiFolderSyncIntervalCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/SetDefaultSyncInterval", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void SetDefaultSyncInterval(int Interval) {
            this.Invoke("SetDefaultSyncInterval", new object[] {
                        Interval});
        }
        public void SetDefaultSyncIntervalAsync(int Interval) {
            this.SetDefaultSyncIntervalAsync(Interval, null);
        }
        public void SetDefaultSyncIntervalAsync(int Interval, object userState) {
            if ((this.SetDefaultSyncIntervalOperationCompleted == null)) {
                this.SetDefaultSyncIntervalOperationCompleted = new System.Threading.SendOrPostCallback(this.OnSetDefaultSyncIntervalOperationCompleted);
            }
            this.InvokeAsync("SetDefaultSyncInterval", new object[] {
                        Interval}, this.SetDefaultSyncIntervalOperationCompleted, userState);
        }
        private void OnSetDefaultSyncIntervalOperationCompleted(object arg) {
            if ((this.SetDefaultSyncIntervalCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.SetDefaultSyncIntervalCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetDefaultSyncInterval", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public int GetDefaultSyncInterval() {
            object[] results = this.Invoke("GetDefaultSyncInterval", new object[0]);
            return ((int)(results[0]));
        }
        public void GetDefaultSyncIntervalAsync() {
            this.GetDefaultSyncIntervalAsync(null);
        }
        public void GetDefaultSyncIntervalAsync(object userState) {
            if ((this.GetDefaultSyncIntervalOperationCompleted == null)) {
                this.GetDefaultSyncIntervalOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetDefaultSyncIntervalOperationCompleted);
            }
            this.InvokeAsync("GetDefaultSyncInterval", new object[0], this.GetDefaultSyncIntervalOperationCompleted, userState);
        }
        private void OnGetDefaultSyncIntervalOperationCompleted(object arg) {
            if ((this.GetDefaultSyncIntervalCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetDefaultSyncIntervalCompleted(this, new GetDefaultSyncIntervalCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/AuthenticateToDomain", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public int AuthenticateToDomain(string DomainID, string Password) {
            object[] results = this.Invoke("AuthenticateToDomain", new object[] {
                        DomainID,
                        Password});
            return ((int)(results[0]));
        }
        public void AuthenticateToDomainAsync(string DomainID, string Password) {
            this.AuthenticateToDomainAsync(DomainID, Password, null);
        }
        public void AuthenticateToDomainAsync(string DomainID, string Password, object userState) {
            if ((this.AuthenticateToDomainOperationCompleted == null)) {
                this.AuthenticateToDomainOperationCompleted = new System.Threading.SendOrPostCallback(this.OnAuthenticateToDomainOperationCompleted);
            }
            this.InvokeAsync("AuthenticateToDomain", new object[] {
                        DomainID,
                        Password}, this.AuthenticateToDomainOperationCompleted, userState);
        }
        private void OnAuthenticateToDomainOperationCompleted(object arg) {
            if ((this.AuthenticateToDomainCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.AuthenticateToDomainCompleted(this, new AuthenticateToDomainCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetiFolderConflicts", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public Conflict[] GetiFolderConflicts(string iFolderID) {
            object[] results = this.Invoke("GetiFolderConflicts", new object[] {
                        iFolderID});
            return ((Conflict[])(results[0]));
        }
        public void GetiFolderConflictsAsync(string iFolderID) {
            this.GetiFolderConflictsAsync(iFolderID, null);
        }
        public void GetiFolderConflictsAsync(string iFolderID, object userState) {
            if ((this.GetiFolderConflictsOperationCompleted == null)) {
                this.GetiFolderConflictsOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetiFolderConflictsOperationCompleted);
            }
            this.InvokeAsync("GetiFolderConflicts", new object[] {
                        iFolderID}, this.GetiFolderConflictsOperationCompleted, userState);
        }
        private void OnGetiFolderConflictsOperationCompleted(object arg) {
            if ((this.GetiFolderConflictsCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetiFolderConflictsCompleted(this, new GetiFolderConflictsCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/ResolveFileConflict", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void ResolveFileConflict(string iFolderID, string conflictID, bool localChangesWin) {
            this.Invoke("ResolveFileConflict", new object[] {
                        iFolderID,
                        conflictID,
                        localChangesWin});
        }
        public void ResolveFileConflictAsync(string iFolderID, string conflictID, bool localChangesWin) {
            this.ResolveFileConflictAsync(iFolderID, conflictID, localChangesWin, null);
        }
        public void ResolveFileConflictAsync(string iFolderID, string conflictID, bool localChangesWin, object userState) {
            if ((this.ResolveFileConflictOperationCompleted == null)) {
                this.ResolveFileConflictOperationCompleted = new System.Threading.SendOrPostCallback(this.OnResolveFileConflictOperationCompleted);
            }
            this.InvokeAsync("ResolveFileConflict", new object[] {
                        iFolderID,
                        conflictID,
                        localChangesWin}, this.ResolveFileConflictOperationCompleted, userState);
        }
        private void OnResolveFileConflictOperationCompleted(object arg) {
            if ((this.ResolveFileConflictCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.ResolveFileConflictCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/ResolveEnhancedFileConflict", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void ResolveEnhancedFileConflict(string iFolderID, string conflictID, bool localChangesWin, string conflictBinPath) {
            this.Invoke("ResolveEnhancedFileConflict", new object[] {
                        iFolderID,
                        conflictID,
                        localChangesWin,
                        conflictBinPath});
        }
        public void ResolveEnhancedFileConflictAsync(string iFolderID, string conflictID, bool localChangesWin, string conflictBinPath) {
            this.ResolveEnhancedFileConflictAsync(iFolderID, conflictID, localChangesWin, conflictBinPath, null);
        }
        public void ResolveEnhancedFileConflictAsync(string iFolderID, string conflictID, bool localChangesWin, string conflictBinPath, object userState) {
            if ((this.ResolveEnhancedFileConflictOperationCompleted == null)) {
                this.ResolveEnhancedFileConflictOperationCompleted = new System.Threading.SendOrPostCallback(this.OnResolveEnhancedFileConflictOperationCompleted);
            }
            this.InvokeAsync("ResolveEnhancedFileConflict", new object[] {
                        iFolderID,
                        conflictID,
                        localChangesWin,
                        conflictBinPath}, this.ResolveEnhancedFileConflictOperationCompleted, userState);
        }
        private void OnResolveEnhancedFileConflictOperationCompleted(object arg) {
            if ((this.ResolveEnhancedFileConflictCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.ResolveEnhancedFileConflictCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/ResolveNameConflict", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void ResolveNameConflict(string iFolderID, string conflictID, string newLocalName) {
            this.Invoke("ResolveNameConflict", new object[] {
                        iFolderID,
                        conflictID,
                        newLocalName});
        }
        public void ResolveNameConflictAsync(string iFolderID, string conflictID, string newLocalName) {
            this.ResolveNameConflictAsync(iFolderID, conflictID, newLocalName, null);
        }
        public void ResolveNameConflictAsync(string iFolderID, string conflictID, string newLocalName, object userState) {
            if ((this.ResolveNameConflictOperationCompleted == null)) {
                this.ResolveNameConflictOperationCompleted = new System.Threading.SendOrPostCallback(this.OnResolveNameConflictOperationCompleted);
            }
            this.InvokeAsync("ResolveNameConflict", new object[] {
                        iFolderID,
                        conflictID,
                        newLocalName}, this.ResolveNameConflictOperationCompleted, userState);
        }
        private void OnResolveNameConflictOperationCompleted(object arg) {
            if ((this.ResolveNameConflictCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.ResolveNameConflictCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/RenameAndResolveConflict", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void RenameAndResolveConflict(string iFolderID, string conflictID, string newFileName) {
            this.Invoke("RenameAndResolveConflict", new object[] {
                        iFolderID,
                        conflictID,
                        newFileName});
        }
        public void RenameAndResolveConflictAsync(string iFolderID, string conflictID, string newFileName) {
            this.RenameAndResolveConflictAsync(iFolderID, conflictID, newFileName, null);
        }
        public void RenameAndResolveConflictAsync(string iFolderID, string conflictID, string newFileName, object userState) {
            if ((this.RenameAndResolveConflictOperationCompleted == null)) {
                this.RenameAndResolveConflictOperationCompleted = new System.Threading.SendOrPostCallback(this.OnRenameAndResolveConflictOperationCompleted);
            }
            this.InvokeAsync("RenameAndResolveConflict", new object[] {
                        iFolderID,
                        conflictID,
                        newFileName}, this.RenameAndResolveConflictOperationCompleted, userState);
        }
        private void OnRenameAndResolveConflictOperationCompleted(object arg) {
            if ((this.RenameAndResolveConflictCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.RenameAndResolveConflictCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/SetupProxy", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void SetupProxy(string Host, int Port) {
            this.Invoke("SetupProxy", new object[] {
                        Host,
                        Port});
        }
        public void SetupProxyAsync(string Host, int Port) {
            this.SetupProxyAsync(Host, Port, null);
        }
        public void SetupProxyAsync(string Host, int Port, object userState) {
            if ((this.SetupProxyOperationCompleted == null)) {
                this.SetupProxyOperationCompleted = new System.Threading.SendOrPostCallback(this.OnSetupProxyOperationCompleted);
            }
            this.InvokeAsync("SetupProxy", new object[] {
                        Host,
                        Port}, this.SetupProxyOperationCompleted, userState);
        }
        private void OnSetupProxyOperationCompleted(object arg) {
            if ((this.SetupProxyCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.SetupProxyCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/RemoveProxy", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void RemoveProxy() {
            this.Invoke("RemoveProxy", new object[0]);
        }
        public void RemoveProxyAsync() {
            this.RemoveProxyAsync(null);
        }
        public void RemoveProxyAsync(object userState) {
            if ((this.RemoveProxyOperationCompleted == null)) {
                this.RemoveProxyOperationCompleted = new System.Threading.SendOrPostCallback(this.OnRemoveProxyOperationCompleted);
            }
            this.InvokeAsync("RemoveProxy", new object[0], this.RemoveProxyOperationCompleted, userState);
        }
        private void OnRemoveProxyOperationCompleted(object arg) {
            if ((this.RemoveProxyCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.RemoveProxyCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/CalculateSyncSize", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public SyncSize CalculateSyncSize(string iFolderID) {
            object[] results = this.Invoke("CalculateSyncSize", new object[] {
                        iFolderID});
            return ((SyncSize)(results[0]));
        }
        public void CalculateSyncSizeAsync(string iFolderID) {
            this.CalculateSyncSizeAsync(iFolderID, null);
        }
        public void CalculateSyncSizeAsync(string iFolderID, object userState) {
            if ((this.CalculateSyncSizeOperationCompleted == null)) {
                this.CalculateSyncSizeOperationCompleted = new System.Threading.SendOrPostCallback(this.OnCalculateSyncSizeOperationCompleted);
            }
            this.InvokeAsync("CalculateSyncSize", new object[] {
                        iFolderID}, this.CalculateSyncSizeOperationCompleted, userState);
        }
        private void OnCalculateSyncSizeOperationCompleted(object arg) {
            if ((this.CalculateSyncSizeCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.CalculateSyncSizeCompleted(this, new CalculateSyncSizeCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/SynciFolderNow", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void SynciFolderNow(string iFolderID) {
            this.Invoke("SynciFolderNow", new object[] {
                        iFolderID});
        }
        public void SynciFolderNowAsync(string iFolderID) {
            this.SynciFolderNowAsync(iFolderID, null);
        }
        public void SynciFolderNowAsync(string iFolderID, object userState) {
            if ((this.SynciFolderNowOperationCompleted == null)) {
                this.SynciFolderNowOperationCompleted = new System.Threading.SendOrPostCallback(this.OnSynciFolderNowOperationCompleted);
            }
            this.InvokeAsync("SynciFolderNow", new object[] {
                        iFolderID}, this.SynciFolderNowOperationCompleted, userState);
        }
        private void OnSynciFolderNowOperationCompleted(object arg) {
            if ((this.SynciFolderNowCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.SynciFolderNowCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/DeleteiFolderFileSizeLimit", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void DeleteiFolderFileSizeLimit(string iFolderID) {
            this.Invoke("DeleteiFolderFileSizeLimit", new object[] {
                        iFolderID});
        }
        public void DeleteiFolderFileSizeLimitAsync(string iFolderID) {
            this.DeleteiFolderFileSizeLimitAsync(iFolderID, null);
        }
        public void DeleteiFolderFileSizeLimitAsync(string iFolderID, object userState) {
            if ((this.DeleteiFolderFileSizeLimitOperationCompleted == null)) {
                this.DeleteiFolderFileSizeLimitOperationCompleted = new System.Threading.SendOrPostCallback(this.OnDeleteiFolderFileSizeLimitOperationCompleted);
            }
            this.InvokeAsync("DeleteiFolderFileSizeLimit", new object[] {
                        iFolderID}, this.DeleteiFolderFileSizeLimitOperationCompleted, userState);
        }
        private void OnDeleteiFolderFileSizeLimitOperationCompleted(object arg) {
            if ((this.DeleteiFolderFileSizeLimitCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.DeleteiFolderFileSizeLimitCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetMemberiFolderFileSizeLimit", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public long GetMemberiFolderFileSizeLimit(string UserID, string iFolderID) {
            object[] results = this.Invoke("GetMemberiFolderFileSizeLimit", new object[] {
                        UserID,
                        iFolderID});
            return ((long)(results[0]));
        }
        public void GetMemberiFolderFileSizeLimitAsync(string UserID, string iFolderID) {
            this.GetMemberiFolderFileSizeLimitAsync(UserID, iFolderID, null);
        }
        public void GetMemberiFolderFileSizeLimitAsync(string UserID, string iFolderID, object userState) {
            if ((this.GetMemberiFolderFileSizeLimitOperationCompleted == null)) {
                this.GetMemberiFolderFileSizeLimitOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetMemberiFolderFileSizeLimitOperationCompleted);
            }
            this.InvokeAsync("GetMemberiFolderFileSizeLimit", new object[] {
                        UserID,
                        iFolderID}, this.GetMemberiFolderFileSizeLimitOperationCompleted, userState);
        }
        private void OnGetMemberiFolderFileSizeLimitOperationCompleted(object arg) {
            if ((this.GetMemberiFolderFileSizeLimitCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetMemberiFolderFileSizeLimitCompleted(this, new GetMemberiFolderFileSizeLimitCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetiFolderFileSizeLimit", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public long GetiFolderFileSizeLimit(string iFolderID) {
            object[] results = this.Invoke("GetiFolderFileSizeLimit", new object[] {
                        iFolderID});
            return ((long)(results[0]));
        }
        public void GetiFolderFileSizeLimitAsync(string iFolderID) {
            this.GetiFolderFileSizeLimitAsync(iFolderID, null);
        }
        public void GetiFolderFileSizeLimitAsync(string iFolderID, object userState) {
            if ((this.GetiFolderFileSizeLimitOperationCompleted == null)) {
                this.GetiFolderFileSizeLimitOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetiFolderFileSizeLimitOperationCompleted);
            }
            this.InvokeAsync("GetiFolderFileSizeLimit", new object[] {
                        iFolderID}, this.GetiFolderFileSizeLimitOperationCompleted, userState);
        }
        private void OnGetiFolderFileSizeLimitOperationCompleted(object arg) {
            if ((this.GetiFolderFileSizeLimitCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetiFolderFileSizeLimitCompleted(this, new GetiFolderFileSizeLimitCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/SetiFolderFileSizeLimit", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public void SetiFolderFileSizeLimit(string iFolderID, long Limit) {
            this.Invoke("SetiFolderFileSizeLimit", new object[] {
                        iFolderID,
                        Limit});
        }
        public void SetiFolderFileSizeLimitAsync(string iFolderID, long Limit) {
            this.SetiFolderFileSizeLimitAsync(iFolderID, Limit, null);
        }
        public void SetiFolderFileSizeLimitAsync(string iFolderID, long Limit, object userState) {
            if ((this.SetiFolderFileSizeLimitOperationCompleted == null)) {
                this.SetiFolderFileSizeLimitOperationCompleted = new System.Threading.SendOrPostCallback(this.OnSetiFolderFileSizeLimitOperationCompleted);
            }
            this.InvokeAsync("SetiFolderFileSizeLimit", new object[] {
                        iFolderID,
                        Limit}, this.SetiFolderFileSizeLimitOperationCompleted, userState);
        }
        private void OnSetiFolderFileSizeLimitOperationCompleted(object arg) {
            if ((this.SetiFolderFileSizeLimitCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.SetiFolderFileSizeLimitCompleted(this, new System.ComponentModel.AsyncCompletedEventArgs(invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/CheckForUpdatedClientAvailable", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public string CheckForUpdatedClientAvailable(string domainID) {
            object[] results = this.Invoke("CheckForUpdatedClientAvailable", new object[] {
                        domainID});
            return ((string)(results[0]));
        }
        public void CheckForUpdatedClientAvailableAsync(string domainID) {
            this.CheckForUpdatedClientAvailableAsync(domainID, null);
        }
        public void CheckForUpdatedClientAvailableAsync(string domainID, object userState) {
            if ((this.CheckForUpdatedClientAvailableOperationCompleted == null)) {
                this.CheckForUpdatedClientAvailableOperationCompleted = new System.Threading.SendOrPostCallback(this.OnCheckForUpdatedClientAvailableOperationCompleted);
            }
            this.InvokeAsync("CheckForUpdatedClientAvailable", new object[] {
                        domainID}, this.CheckForUpdatedClientAvailableOperationCompleted, userState);
        }
        private void OnCheckForUpdatedClientAvailableOperationCompleted(object arg) {
            if ((this.CheckForUpdatedClientAvailableCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.CheckForUpdatedClientAvailableCompleted(this, new CheckForUpdatedClientAvailableCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/CheckForUpdatedClient", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public string CheckForUpdatedClient(string domainID) {
            object[] results = this.Invoke("CheckForUpdatedClient", new object[] {
                        domainID});
            return ((string)(results[0]));
        }
        public void CheckForUpdatedClientAsync(string domainID) {
            this.CheckForUpdatedClientAsync(domainID, null);
        }
        public void CheckForUpdatedClientAsync(string domainID, object userState) {
            if ((this.CheckForUpdatedClientOperationCompleted == null)) {
                this.CheckForUpdatedClientOperationCompleted = new System.Threading.SendOrPostCallback(this.OnCheckForUpdatedClientOperationCompleted);
            }
            this.InvokeAsync("CheckForUpdatedClient", new object[] {
                        domainID}, this.CheckForUpdatedClientOperationCompleted, userState);
        }
        private void OnCheckForUpdatedClientOperationCompleted(object arg) {
            if ((this.CheckForUpdatedClientCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.CheckForUpdatedClientCompleted(this, new CheckForUpdatedClientCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/CheckForUpdate", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public int CheckForUpdate(string domainID, out string ServerVersion) {
            object[] results = this.Invoke("CheckForUpdate", new object[] {
                        domainID});
            ServerVersion = ((string)(results[1]));
            return ((int)(results[0]));
        }
        public void CheckForUpdateAsync(string domainID) {
            this.CheckForUpdateAsync(domainID, null);
        }
        public void CheckForUpdateAsync(string domainID, object userState) {
            if ((this.CheckForUpdateOperationCompleted == null)) {
                this.CheckForUpdateOperationCompleted = new System.Threading.SendOrPostCallback(this.OnCheckForUpdateOperationCompleted);
            }
            this.InvokeAsync("CheckForUpdate", new object[] {
                        domainID}, this.CheckForUpdateOperationCompleted, userState);
        }
        private void OnCheckForUpdateOperationCompleted(object arg) {
            if ((this.CheckForUpdateCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.CheckForUpdateCompleted(this, new CheckForUpdateCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/CheckForServerUpdate", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public bool CheckForServerUpdate(string domainID) {
            object[] results = this.Invoke("CheckForServerUpdate", new object[] {
                        domainID});
            return ((bool)(results[0]));
        }
        public void CheckForServerUpdateAsync(string domainID) {
            this.CheckForServerUpdateAsync(domainID, null);
        }
        public void CheckForServerUpdateAsync(string domainID, object userState) {
            if ((this.CheckForServerUpdateOperationCompleted == null)) {
                this.CheckForServerUpdateOperationCompleted = new System.Threading.SendOrPostCallback(this.OnCheckForServerUpdateOperationCompleted);
            }
            this.InvokeAsync("CheckForServerUpdate", new object[] {
                        domainID}, this.CheckForServerUpdateOperationCompleted, userState);
        }
        private void OnCheckForServerUpdateOperationCompleted(object arg) {
            if ((this.CheckForServerUpdateCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.CheckForServerUpdateCompleted(this, new CheckForServerUpdateCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/RunClientUpdate", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public bool RunClientUpdate(string domainID, string path) {
            object[] results = this.Invoke("RunClientUpdate", new object[] {
                        domainID,
                        path});
            return ((bool)(results[0]));
        }
        public void RunClientUpdateAsync(string domainID, string path) {
            this.RunClientUpdateAsync(domainID, path, null);
        }
        public void RunClientUpdateAsync(string domainID, string path, object userState) {
            if ((this.RunClientUpdateOperationCompleted == null)) {
                this.RunClientUpdateOperationCompleted = new System.Threading.SendOrPostCallback(this.OnRunClientUpdateOperationCompleted);
            }
            this.InvokeAsync("RunClientUpdate", new object[] {
                        domainID,
                        path}, this.RunClientUpdateOperationCompleted, userState);
        }
        private void OnRunClientUpdateOperationCompleted(object arg) {
            if ((this.RunClientUpdateCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.RunClientUpdateCompleted(this, new RunClientUpdateCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/ChangePassword", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public int ChangePassword(string domainid, string oldpassword, string newpassword) {
            object[] results = this.Invoke("ChangePassword", new object[] {
                        domainid,
                        oldpassword,
                        newpassword});
            return ((int)(results[0]));
        }
        public void ChangePasswordAsync(string domainid, string oldpassword, string newpassword) {
            this.ChangePasswordAsync(domainid, oldpassword, newpassword, null);
        }
        public void ChangePasswordAsync(string domainid, string oldpassword, string newpassword, object userState) {
            if ((this.ChangePasswordOperationCompleted == null)) {
                this.ChangePasswordOperationCompleted = new System.Threading.SendOrPostCallback(this.OnChangePasswordOperationCompleted);
            }
            this.InvokeAsync("ChangePassword", new object[] {
                        domainid,
                        oldpassword,
                        newpassword}, this.ChangePasswordOperationCompleted, userState);
        }
        private void OnChangePasswordOperationCompleted(object arg) {
            if ((this.ChangePasswordCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.ChangePasswordCompleted(this, new ChangePasswordCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        [System.Web.Services.Protocols.SoapDocumentMethodAttribute("http://novell.com/ifolder/web/GetDefaultServerPublicKey", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/", Use=System.Web.Services.Description.SoapBindingUse.Literal, ParameterStyle=System.Web.Services.Protocols.SoapParameterStyle.Wrapped)]
        public string GetDefaultServerPublicKey(string DomainID, string UserID) {
            object[] results = this.Invoke("GetDefaultServerPublicKey", new object[] {
                        DomainID,
                        UserID});
            return ((string)(results[0]));
        }
        public void GetDefaultServerPublicKeyAsync(string DomainID, string UserID) {
            this.GetDefaultServerPublicKeyAsync(DomainID, UserID, null);
        }
        public void GetDefaultServerPublicKeyAsync(string DomainID, string UserID, object userState) {
            if ((this.GetDefaultServerPublicKeyOperationCompleted == null)) {
                this.GetDefaultServerPublicKeyOperationCompleted = new System.Threading.SendOrPostCallback(this.OnGetDefaultServerPublicKeyOperationCompleted);
            }
            this.InvokeAsync("GetDefaultServerPublicKey", new object[] {
                        DomainID,
                        UserID}, this.GetDefaultServerPublicKeyOperationCompleted, userState);
        }
        private void OnGetDefaultServerPublicKeyOperationCompleted(object arg) {
            if ((this.GetDefaultServerPublicKeyCompleted != null)) {
                System.Web.Services.Protocols.InvokeCompletedEventArgs invokeArgs = ((System.Web.Services.Protocols.InvokeCompletedEventArgs)(arg));
                this.GetDefaultServerPublicKeyCompleted(this, new GetDefaultServerPublicKeyCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        public new void CancelAsync(object userState) {
            base.CancelAsync(userState);
        }
        private bool IsLocalFileSystemWebService(string url) {
            if (((url == null)
                        || (url == string.Empty))) {
                return false;
            }
            System.Uri wsUri = new System.Uri(url);
            if (((wsUri.Port >= 1024)
                        && (string.Compare(wsUri.Host, "localHost", System.StringComparison.OrdinalIgnoreCase) == 0))) {
                return true;
            }
            return false;
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Xml", "2.0.50727.1433")]
    [System.SerializableAttribute()]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    [System.Xml.Serialization.XmlTypeAttribute(Namespace="http://novell.com/ifolder/web/")]
    public partial class iFolderWeb {
        private string domainIDField;
        private string idField;
        private ulong localIncarnationField;
        private string managedPathField;
        private string unManagedPathField;
        private ulong masterIncarnationField;
        private string nameField;
        private string ownerField;
        private string ownerIDField;
        private int effectiveSyncIntervalField;
        private int syncIntervalField;
        private bool synchronizableField;
        private string typeField;
        private string descriptionField;
        private string stateField;
        private bool isSubscriptionField;
        private int enumeratedStateField;
        private bool isWorkgroupField;
        private bool hasConflictsField;
        private string currentUserIDField;
        private string currentUserRightsField;
        private string collectionIDField;
        private string lastSyncTimeField;
        private string roleField;
        private bool sslField;
        private string encryptionAlgorithmField;
        private int migratediFolderField;
        private bool sharedField;
        private long iFolderSizeField;
        public string DomainID {
            get {
                return this.domainIDField;
            }
            set {
                this.domainIDField = value;
            }
        }
        public string ID {
            get {
                return this.idField;
            }
            set {
                this.idField = value;
            }
        }
        public ulong LocalIncarnation {
            get {
                return this.localIncarnationField;
            }
            set {
                this.localIncarnationField = value;
            }
        }
        public string ManagedPath {
            get {
                return this.managedPathField;
            }
            set {
                this.managedPathField = value;
            }
        }
        public string UnManagedPath {
            get {
                return this.unManagedPathField;
            }
            set {
                this.unManagedPathField = value;
            }
        }
        public ulong MasterIncarnation {
            get {
                return this.masterIncarnationField;
            }
            set {
                this.masterIncarnationField = value;
            }
        }
        public string Name {
            get {
                return this.nameField;
            }
            set {
                this.nameField = value;
            }
        }
        public string Owner {
            get {
                return this.ownerField;
            }
            set {
                this.ownerField = value;
            }
        }
        public string OwnerID {
            get {
                return this.ownerIDField;
            }
            set {
                this.ownerIDField = value;
            }
        }
        public int EffectiveSyncInterval {
            get {
                return this.effectiveSyncIntervalField;
            }
            set {
                this.effectiveSyncIntervalField = value;
            }
        }
        public int SyncInterval {
            get {
                return this.syncIntervalField;
            }
            set {
                this.syncIntervalField = value;
            }
        }
        public bool Synchronizable {
            get {
                return this.synchronizableField;
            }
            set {
                this.synchronizableField = value;
            }
        }
        public string Type {
            get {
                return this.typeField;
            }
            set {
                this.typeField = value;
            }
        }
        public string Description {
            get {
                return this.descriptionField;
            }
            set {
                this.descriptionField = value;
            }
        }
        public string State {
            get {
                return this.stateField;
            }
            set {
                this.stateField = value;
            }
        }
        public bool IsSubscription {
            get {
                return this.isSubscriptionField;
            }
            set {
                this.isSubscriptionField = value;
            }
        }
        public int EnumeratedState {
            get {
                return this.enumeratedStateField;
            }
            set {
                this.enumeratedStateField = value;
            }
        }
        public bool IsWorkgroup {
            get {
                return this.isWorkgroupField;
            }
            set {
                this.isWorkgroupField = value;
            }
        }
        public bool HasConflicts {
            get {
                return this.hasConflictsField;
            }
            set {
                this.hasConflictsField = value;
            }
        }
        public string CurrentUserID {
            get {
                return this.currentUserIDField;
            }
            set {
                this.currentUserIDField = value;
            }
        }
        public string CurrentUserRights {
            get {
                return this.currentUserRightsField;
            }
            set {
                this.currentUserRightsField = value;
            }
        }
        public string CollectionID {
            get {
                return this.collectionIDField;
            }
            set {
                this.collectionIDField = value;
            }
        }
        public string LastSyncTime {
            get {
                return this.lastSyncTimeField;
            }
            set {
                this.lastSyncTimeField = value;
            }
        }
        public string Role {
            get {
                return this.roleField;
            }
            set {
                this.roleField = value;
            }
        }
        public bool ssl {
            get {
                return this.sslField;
            }
            set {
                this.sslField = value;
            }
        }
        public string encryptionAlgorithm {
            get {
                return this.encryptionAlgorithmField;
            }
            set {
                this.encryptionAlgorithmField = value;
            }
        }
        public int MigratediFolder {
            get {
                return this.migratediFolderField;
            }
            set {
                this.migratediFolderField = value;
            }
        }
        public bool shared {
            get {
                return this.sharedField;
            }
            set {
                this.sharedField = value;
            }
        }
        public long iFolderSize {
            get {
                return this.iFolderSizeField;
            }
            set {
                this.iFolderSizeField = value;
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Xml", "2.0.50727.1433")]
    [System.SerializableAttribute()]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    [System.Xml.Serialization.XmlTypeAttribute(Namespace="http://novell.com/ifolder/web/")]
    public partial class SyncSize {
        private uint syncNodeCountField;
        private ulong syncByteCountField;
        public uint SyncNodeCount {
            get {
                return this.syncNodeCountField;
            }
            set {
                this.syncNodeCountField = value;
            }
        }
        public ulong SyncByteCount {
            get {
                return this.syncByteCountField;
            }
            set {
                this.syncByteCountField = value;
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Xml", "2.0.50727.1433")]
    [System.SerializableAttribute()]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    [System.Xml.Serialization.XmlTypeAttribute(Namespace="http://novell.com/ifolder/web/")]
    public partial class Conflict {
        private string iFolderIDField;
        private string conflictIDField;
        private string localNameField;
        private string localDateField;
        private string localSizeField;
        private string localFullPathField;
        private bool isNameConflictField;
        private string serverNameField;
        private string serverDateField;
        private string serverSizeField;
        private string serverFullPathField;
        public string iFolderID {
            get {
                return this.iFolderIDField;
            }
            set {
                this.iFolderIDField = value;
            }
        }
        public string ConflictID {
            get {
                return this.conflictIDField;
            }
            set {
                this.conflictIDField = value;
            }
        }
        public string LocalName {
            get {
                return this.localNameField;
            }
            set {
                this.localNameField = value;
            }
        }
        public string LocalDate {
            get {
                return this.localDateField;
            }
            set {
                this.localDateField = value;
            }
        }
        public string LocalSize {
            get {
                return this.localSizeField;
            }
            set {
                this.localSizeField = value;
            }
        }
        public string LocalFullPath {
            get {
                return this.localFullPathField;
            }
            set {
                this.localFullPathField = value;
            }
        }
        public bool IsNameConflict {
            get {
                return this.isNameConflictField;
            }
            set {
                this.isNameConflictField = value;
            }
        }
        public string ServerName {
            get {
                return this.serverNameField;
            }
            set {
                this.serverNameField = value;
            }
        }
        public string ServerDate {
            get {
                return this.serverDateField;
            }
            set {
                this.serverDateField = value;
            }
        }
        public string ServerSize {
            get {
                return this.serverSizeField;
            }
            set {
                this.serverSizeField = value;
            }
        }
        public string ServerFullPath {
            get {
                return this.serverFullPathField;
            }
            set {
                this.serverFullPathField = value;
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Xml", "2.0.50727.1433")]
    [System.SerializableAttribute()]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    [System.Xml.Serialization.XmlTypeAttribute(Namespace="http://novell.com/ifolder/web/")]
    public partial class DiskSpace {
        private long availableSpaceField;
        private long limitField;
        private long usedSpaceField;
        public long AvailableSpace {
            get {
                return this.availableSpaceField;
            }
            set {
                this.availableSpaceField = value;
            }
        }
        public long Limit {
            get {
                return this.limitField;
            }
            set {
                this.limitField = value;
            }
        }
        public long UsedSpace {
            get {
                return this.usedSpaceField;
            }
            set {
                this.usedSpaceField = value;
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Xml", "2.0.50727.1433")]
    [System.SerializableAttribute()]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    [System.Xml.Serialization.XmlTypeAttribute(Namespace="http://novell.com/ifolder/web/")]
    public partial class iFolderUser {
        private string nameField;
        private string userIDField;
        private string rightsField;
        private string idField;
        private string stateField;
        private string iFolderIDField;
        private bool isOwnerField;
        private string firstNameField;
        private string surnameField;
        private string fnField;
        public string Name {
            get {
                return this.nameField;
            }
            set {
                this.nameField = value;
            }
        }
        public string UserID {
            get {
                return this.userIDField;
            }
            set {
                this.userIDField = value;
            }
        }
        public string Rights {
            get {
                return this.rightsField;
            }
            set {
                this.rightsField = value;
            }
        }
        public string ID {
            get {
                return this.idField;
            }
            set {
                this.idField = value;
            }
        }
        public string State {
            get {
                return this.stateField;
            }
            set {
                this.stateField = value;
            }
        }
        public string iFolderID {
            get {
                return this.iFolderIDField;
            }
            set {
                this.iFolderIDField = value;
            }
        }
        public bool IsOwner {
            get {
                return this.isOwnerField;
            }
            set {
                this.isOwnerField = value;
            }
        }
        public string FirstName {
            get {
                return this.firstNameField;
            }
            set {
                this.firstNameField = value;
            }
        }
        public string Surname {
            get {
                return this.surnameField;
            }
            set {
                this.surnameField = value;
            }
        }
        public string FN {
            get {
                return this.fnField;
            }
            set {
                this.fnField = value;
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Xml", "2.0.50727.1433")]
    [System.SerializableAttribute()]
    [System.Xml.Serialization.XmlTypeAttribute(Namespace="http://novell.com/ifolder/web/")]
    public enum iFolderSearchType {
        Equal,
        Not_Equal,
        Begins,
        Ends,
        Contains,
        Greater,
        Less,
        Greater_Equal,
        Less_Equal,
        Exists,
        CaseEqual,
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void PingCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void IsiFolderCompletedEventHandler(object sender, IsiFolderCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class IsiFolderCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal IsiFolderCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public bool Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((bool)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void CanBeiFolderCompletedEventHandler(object sender, CanBeiFolderCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class CanBeiFolderCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal CanBeiFolderCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public bool Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((bool)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void IsPathIniFolderCompletedEventHandler(object sender, IsPathIniFolderCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class IsPathIniFolderCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal IsPathIniFolderCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public bool Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((bool)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void CreateLocaliFolderCompletedEventHandler(object sender, CreateLocaliFolderCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class CreateLocaliFolderCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal CreateLocaliFolderCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderWeb Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderWeb)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void CreateiFolderInDomainEncrCompletedEventHandler(object sender, CreateiFolderInDomainEncrCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class CreateiFolderInDomainEncrCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal CreateiFolderInDomainEncrCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderWeb Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderWeb)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void CreateiFolderInDomainCompletedEventHandler(object sender, CreateiFolderInDomainCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class CreateiFolderInDomainCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal CreateiFolderInDomainCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderWeb Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderWeb)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void CheckiFolderCompletedEventHandler(object sender, CheckiFolderCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class CheckiFolderCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal CheckiFolderCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public bool Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((bool)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetiFolderCompletedEventHandler(object sender, GetiFolderCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetiFolderCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetiFolderCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderWeb Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderWeb)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetMinimaliFolderCompletedEventHandler(object sender, GetMinimaliFolderCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetMinimaliFolderCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetMinimaliFolderCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderWeb Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderWeb)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetSecurityPolicyCompletedEventHandler(object sender, GetSecurityPolicyCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetSecurityPolicyCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetSecurityPolicyCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public int Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((int)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetDisableSharingPolicyCompletedEventHandler(object sender, GetDisableSharingPolicyCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetDisableSharingPolicyCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetDisableSharingPolicyCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public bool Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((bool)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void CanOwnerBeChangedCompletedEventHandler(object sender, CanOwnerBeChangedCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class CanOwnerBeChangedCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal CanOwnerBeChangedCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public bool Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((bool)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetLimitPolicyStatusCompletedEventHandler(object sender, GetLimitPolicyStatusCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetLimitPolicyStatusCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetLimitPolicyStatusCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public int Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((int)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetiFolderInvitationCompletedEventHandler(object sender, GetiFolderInvitationCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetiFolderInvitationCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetiFolderInvitationCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderWeb Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderWeb)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetiFolderByLocalPathCompletedEventHandler(object sender, GetiFolderByLocalPathCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetiFolderByLocalPathCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetiFolderByLocalPathCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderWeb Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderWeb)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void DeleteiFolderCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void RevertiFolderCompletedEventHandler(object sender, RevertiFolderCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class RevertiFolderCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal RevertiFolderCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderWeb Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderWeb)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void RevertiFolder1CompletedEventHandler(object sender, RevertiFolder1CompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class RevertiFolder1CompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal RevertiFolder1CompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderWeb Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderWeb)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetAlliFoldersCompletedEventHandler(object sender, GetAlliFoldersCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetAlliFoldersCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetAlliFoldersCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderWeb[] Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderWeb[])(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetAlliFolders1CompletedEventHandler(object sender, GetAlliFolders1CompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetAlliFolders1CompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetAlliFolders1CompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderWeb[] Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderWeb[])(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetiFoldersForDomainCompletedEventHandler(object sender, GetiFoldersForDomainCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetiFoldersForDomainCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetiFoldersForDomainCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderWeb[] Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderWeb[])(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetiFoldersCompletedEventHandler(object sender, GetiFoldersCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetiFoldersCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetiFoldersCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderWeb[] Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderWeb[])(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void SetUserRightsCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetOwnerCompletedEventHandler(object sender, GetOwnerCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetOwnerCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetOwnerCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderUser Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderUser)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void ChangeOwnerCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void RemoveiFolderUserCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetiFolderUsersCompletedEventHandler(object sender, GetiFolderUsersCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetiFolderUsersCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetiFolderUsersCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderUser[] Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderUser[])(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetDomainUsersCompletedEventHandler(object sender, GetDomainUsersCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetDomainUsersCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetDomainUsersCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderUser[] Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderUser[])(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void SearchForDomainUsersCompletedEventHandler(object sender, SearchForDomainUsersCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class SearchForDomainUsersCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal SearchForDomainUsersCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderUser[] Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderUser[])(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void FindCloseiFolderMembersCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void FindFirstiFolderMembersCompletedEventHandler(object sender, FindFirstiFolderMembersCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class FindFirstiFolderMembersCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal FindFirstiFolderMembersCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public bool Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((bool)(this.results[0]));
            }
        }
        public string searchContext {
            get {
                this.RaiseExceptionIfNecessary();
                return ((string)(this.results[1]));
            }
        }
        public iFolderUser[] memberList {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderUser[])(this.results[2]));
            }
        }
        public int totalMembers {
            get {
                this.RaiseExceptionIfNecessary();
                return ((int)(this.results[3]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void FindFirstSpecificiFolderMembersCompletedEventHandler(object sender, FindFirstSpecificiFolderMembersCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class FindFirstSpecificiFolderMembersCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal FindFirstSpecificiFolderMembersCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public bool Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((bool)(this.results[0]));
            }
        }
        public string searchContext {
            get {
                this.RaiseExceptionIfNecessary();
                return ((string)(this.results[1]));
            }
        }
        public iFolderUser[] memberList {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderUser[])(this.results[2]));
            }
        }
        public int totalMembers {
            get {
                this.RaiseExceptionIfNecessary();
                return ((int)(this.results[3]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void FindNextiFolderMembersCompletedEventHandler(object sender, FindNextiFolderMembersCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class FindNextiFolderMembersCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal FindNextiFolderMembersCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public bool Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((bool)(this.results[0]));
            }
        }
        public string searchContext {
            get {
                this.RaiseExceptionIfNecessary();
                return ((string)(this.results[1]));
            }
        }
        public iFolderUser[] memberList {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderUser[])(this.results[2]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void FindPreviousiFolderMembersCompletedEventHandler(object sender, FindPreviousiFolderMembersCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class FindPreviousiFolderMembersCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal FindPreviousiFolderMembersCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public bool Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((bool)(this.results[0]));
            }
        }
        public string searchContext {
            get {
                this.RaiseExceptionIfNecessary();
                return ((string)(this.results[1]));
            }
        }
        public iFolderUser[] memberList {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderUser[])(this.results[2]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void FindSeekiFolderMembersCompletedEventHandler(object sender, FindSeekiFolderMembersCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class FindSeekiFolderMembersCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal FindSeekiFolderMembersCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public bool Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((bool)(this.results[0]));
            }
        }
        public string searchContext {
            get {
                this.RaiseExceptionIfNecessary();
                return ((string)(this.results[1]));
            }
        }
        public iFolderUser[] memberList {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderUser[])(this.results[2]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetiFolderUserCompletedEventHandler(object sender, GetiFolderUserCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetiFolderUserCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetiFolderUserCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderUser Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderUser)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetRANameCompletedEventHandler(object sender, GetRANameCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetRANameCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetRANameCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public string Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((string)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetiFolderUserFromNodeIDCompletedEventHandler(object sender, GetiFolderUserFromNodeIDCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetiFolderUserFromNodeIDCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetiFolderUserFromNodeIDCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderUser Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderUser)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void AddAndInviteUserCompletedEventHandler(object sender, AddAndInviteUserCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class AddAndInviteUserCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal AddAndInviteUserCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderUser Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderUser)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void InviteUserCompletedEventHandler(object sender, InviteUserCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class InviteUserCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal InviteUserCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderUser Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderUser)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void MergeiFolderCompletedEventHandler(object sender, MergeiFolderCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class MergeiFolderCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal MergeiFolderCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderWeb Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderWeb)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void AcceptiFolderInvitationCompletedEventHandler(object sender, AcceptiFolderInvitationCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class AcceptiFolderInvitationCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal AcceptiFolderInvitationCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderWeb Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderWeb)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void AcceptiFolderInvitation1CompletedEventHandler(object sender, AcceptiFolderInvitation1CompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class AcceptiFolderInvitation1CompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal AcceptiFolderInvitation1CompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public iFolderWeb Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((iFolderWeb)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void CheckForMacUpdateCompletedEventHandler(object sender, CheckForMacUpdateCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class CheckForMacUpdateCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal CheckForMacUpdateCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public int Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((int)(this.results[0]));
            }
        }
        public string ServerVersion {
            get {
                this.RaiseExceptionIfNecessary();
                return ((string)(this.results[1]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void DeclineiFolderInvitationCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetUserDiskSpaceCompletedEventHandler(object sender, GetUserDiskSpaceCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetUserDiskSpaceCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetUserDiskSpaceCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public DiskSpace Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((DiskSpace)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetiFolderDiskSpaceCompletedEventHandler(object sender, GetiFolderDiskSpaceCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetiFolderDiskSpaceCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetiFolderDiskSpaceCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public DiskSpace Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((DiskSpace)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void SetUserDiskSpaceLimitCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void SetiFolderDiskSpaceLimitCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void SetiFolderSecureSyncCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void SetiFolderSyncIntervalCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void SetDefaultSyncIntervalCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetDefaultSyncIntervalCompletedEventHandler(object sender, GetDefaultSyncIntervalCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetDefaultSyncIntervalCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetDefaultSyncIntervalCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public int Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((int)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void AuthenticateToDomainCompletedEventHandler(object sender, AuthenticateToDomainCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class AuthenticateToDomainCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal AuthenticateToDomainCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public int Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((int)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetiFolderConflictsCompletedEventHandler(object sender, GetiFolderConflictsCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetiFolderConflictsCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetiFolderConflictsCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public Conflict[] Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((Conflict[])(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void ResolveFileConflictCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void ResolveEnhancedFileConflictCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void ResolveNameConflictCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void RenameAndResolveConflictCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void SetupProxyCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void RemoveProxyCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void CalculateSyncSizeCompletedEventHandler(object sender, CalculateSyncSizeCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class CalculateSyncSizeCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal CalculateSyncSizeCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public SyncSize Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((SyncSize)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void SynciFolderNowCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void DeleteiFolderFileSizeLimitCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetMemberiFolderFileSizeLimitCompletedEventHandler(object sender, GetMemberiFolderFileSizeLimitCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetMemberiFolderFileSizeLimitCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetMemberiFolderFileSizeLimitCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public long Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((long)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetiFolderFileSizeLimitCompletedEventHandler(object sender, GetiFolderFileSizeLimitCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetiFolderFileSizeLimitCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetiFolderFileSizeLimitCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public long Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((long)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void SetiFolderFileSizeLimitCompletedEventHandler(object sender, System.ComponentModel.AsyncCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void CheckForUpdatedClientAvailableCompletedEventHandler(object sender, CheckForUpdatedClientAvailableCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class CheckForUpdatedClientAvailableCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal CheckForUpdatedClientAvailableCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public string Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((string)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void CheckForUpdatedClientCompletedEventHandler(object sender, CheckForUpdatedClientCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class CheckForUpdatedClientCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal CheckForUpdatedClientCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public string Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((string)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void CheckForUpdateCompletedEventHandler(object sender, CheckForUpdateCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class CheckForUpdateCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal CheckForUpdateCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public int Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((int)(this.results[0]));
            }
        }
        public string ServerVersion {
            get {
                this.RaiseExceptionIfNecessary();
                return ((string)(this.results[1]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void CheckForServerUpdateCompletedEventHandler(object sender, CheckForServerUpdateCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class CheckForServerUpdateCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal CheckForServerUpdateCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public bool Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((bool)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void RunClientUpdateCompletedEventHandler(object sender, RunClientUpdateCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class RunClientUpdateCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal RunClientUpdateCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public bool Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((bool)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void ChangePasswordCompletedEventHandler(object sender, ChangePasswordCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class ChangePasswordCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal ChangePasswordCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public int Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((int)(this.results[0]));
            }
        }
    }
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    public delegate void GetDefaultServerPublicKeyCompletedEventHandler(object sender, GetDefaultServerPublicKeyCompletedEventArgs e);
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Web.Services", "2.0.50727.1433")]
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.ComponentModel.DesignerCategoryAttribute("code")]
    public partial class GetDefaultServerPublicKeyCompletedEventArgs : System.ComponentModel.AsyncCompletedEventArgs {
        private object[] results;
        internal GetDefaultServerPublicKeyCompletedEventArgs(object[] results, System.Exception exception, bool cancelled, object userState) :
                base(exception, cancelled, userState) {
            this.results = results;
        }
        public string Result {
            get {
                this.RaiseExceptionIfNecessary();
                return ((string)(this.results[0]));
            }
        }
    }
}
