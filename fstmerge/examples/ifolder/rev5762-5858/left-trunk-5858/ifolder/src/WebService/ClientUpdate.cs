using System.Diagnostics;
using System.Xml.Serialization;
using System;
using System.Web.Services.Protocols;
using System.ComponentModel;
using System.Web.Services;
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Web.Services.WebServiceBindingAttribute(Name="ClientUpdateSoap", Namespace="http://novell.com/ifolder/web/")]
public class ClientUpdate : System.Web.Services.Protocols.SoapHttpClientProtocol {
    public ClientUpdate() {
        this.Url = "http://127.0.0.1:4909/simias10/mlasky/ClientUpdate.asmx";
    }
    [System.Web.Services.Protocols.SoapRpcMethodAttribute("http://novell.com/ifolder/web/GetUpdateFiles", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/")]
    public string[] GetUpdateFiles() {
        object[] results = this.Invoke("GetUpdateFiles", new object[0]);
        return ((string[])(results[0]));
    }
    public System.IAsyncResult BeginGetUpdateFiles(System.AsyncCallback callback, object asyncState) {
        return this.BeginInvoke("GetUpdateFiles", new object[0], callback, asyncState);
    }
    public string[] EndGetUpdateFiles(System.IAsyncResult asyncResult) {
        object[] results = this.EndInvoke(asyncResult);
        return ((string[])(results[0]));
    }
    [System.Web.Services.Protocols.SoapRpcMethodAttribute("http://novell.com/ifolder/web/IsUpdateAvailable", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/")]
    public string IsUpdateAvailable(string platform, string currentVersion) {
        object[] results = this.Invoke("IsUpdateAvailable", new object[] {
                    platform,
                    currentVersion});
        return ((string)(results[0]));
    }
    public System.IAsyncResult BeginIsUpdateAvailable(string platform, string currentVersion, System.AsyncCallback callback, object asyncState) {
        return this.BeginInvoke("IsUpdateAvailable", new object[] {
                    platform,
                    currentVersion}, callback, asyncState);
    }
    public string EndIsUpdateAvailable(System.IAsyncResult asyncResult) {
        object[] results = this.EndInvoke(asyncResult);
        return ((string)(results[0]));
    }
}
