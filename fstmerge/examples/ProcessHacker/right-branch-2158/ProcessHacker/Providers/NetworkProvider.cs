




using System;
using System.Collections.Generic;
using System.Net;
using System.Net.Sockets;
using ProcessHacker.Common;
using ProcessHacker.Common.Messaging;
using ProcessHacker.Native;

namespace ProcessHacker
{
    public class NetworkItem : ICloneable
    {
        public object Clone()
        {
            return this.MemberwiseClone();
        }

        public int Tag;
        public string Id;
        public NetworkConnection Connection;
        public string LocalString;
        public string RemoteString;
        public bool LocalTouched;
        public bool RemoteTouched;
        public bool JustProcessed;
    }

    public class NetworkProvider : Provider<string, NetworkItem>
    {
        private class AddressResolveMessage : Message
        {
            public string Id;
            public bool Remote;
            public string HostName;
        }

        private MessageQueue _messageQueue = new MessageQueue();
        private Dictionary<IPAddress, string> _resolveCache = new Dictionary<IPAddress, string>();

        public NetworkProvider()
            : base()
        {
            this.Name = this.GetType().Name;
            this.ProviderUpdate += new ProviderUpdateOnce(UpdateOnce);

            _messageQueue.AddListener(
                new MessageQueueListener<AddressResolveMessage>((message) =>
                {
                    if (Dictionary.ContainsKey(message.Id))
                    {
                        var item = Dictionary[message.Id];

                        if (message.Remote)
                            item.RemoteString = message.HostName;
                        else
                            item.LocalString = message.HostName;

                        item.JustProcessed = true;
                    }
                }));
        }

        private void UpdateOnce()
        {
            var networkDict = Windows.GetNetworkConnections();
            var preKeyDict = new Dictionary<string, KeyValuePair<int, NetworkConnection> >();
            var keyDict = new Dictionary<string, NetworkItem>();
            Dictionary<string, NetworkItem> newDict =
                new Dictionary<string, NetworkItem>(this.Dictionary);


            foreach (var list in networkDict.Values)
            {
                foreach (var connection in list)
                {
                    if (connection.Pid == Program.CurrentProcessId &&
                        Properties.Settings.Default.HideProcessHackerNetworkConnections)
                        continue;

                    string id = connection.Pid.ToString() + "-" + connection.Local.ToString() + "-" +
                        (connection.Remote != null ? connection.Remote.ToString() : "") + "-" + connection.Protocol.ToString();

                    if (preKeyDict.ContainsKey(id))
                        preKeyDict[id] = new KeyValuePair<int, NetworkConnection>(
                            preKeyDict[id].Key + 1, preKeyDict[id].Value);
                    else
                        preKeyDict.Add(id, new KeyValuePair<int, NetworkConnection>(1, connection));
                }
            }


            foreach (string s in preKeyDict.Keys)
            {
                var connection = preKeyDict[s].Value;
                NetworkItem item = new NetworkItem();

                item.Id = s + "-" + preKeyDict[s].Key.ToString();
                item.Connection = connection;
                keyDict.Add(s + "-" + preKeyDict[s].Key.ToString(), item);
            }

            foreach (var connection in this.Dictionary.Values)
            {
                if (!keyDict.ContainsKey(connection.Id))
                {
                    OnDictionaryRemoved(connection);
                    newDict.Remove(connection.Id);
                }
            }


            _messageQueue.Listen();

            foreach (var connection in keyDict.Values)
            {
                if (!this.Dictionary.ContainsKey(connection.Id))
                {
                    connection.Tag = this.RunCount;


                    if (connection.Connection.Local != null)
                    {
                        if (!connection.Connection.Local.Address.GetAddressBytes().IsEmpty())
                        {

                            lock (_resolveCache)
                            {
                                if (_resolveCache.ContainsKey(connection.Connection.Local.Address))
                                {

                                    connection.LocalString = _resolveCache[connection.Connection.Local.Address];
                                }
                                else
                                {

                                    WorkQueue.GlobalQueueWorkItemTag(
                                        new Action<string, bool, IPAddress>(this.ResolveAddresses),
                                        "network-resolve-local",
                                        connection.Id,
                                        false,
                                        connection.Connection.Local.Address
                                        );
                                }
                            }
                        }
                    }

                    if (connection.Connection.Remote != null)
                    {
                        if (!connection.Connection.Remote.Address.GetAddressBytes().IsEmpty())
                        {
                            lock (_resolveCache)
                            {
                                if (_resolveCache.ContainsKey(connection.Connection.Remote.Address))
                                {

                                    connection.RemoteString = _resolveCache[connection.Connection.Remote.Address];
                                }
                                else
                                {
                                    WorkQueue.GlobalQueueWorkItemTag(
                                        new Action<string, bool, IPAddress>(this.ResolveAddresses),
                                        "network-resolve-remote",
                                        connection.Id,
                                        true,
                                        connection.Connection.Remote.Address
                                        );
                                }
                            }
                        }
                    }


                    newDict.Add(connection.Id, connection);
                    OnDictionaryAdded(connection);
                }
                else
                {
                    if (
                        connection.Connection.State != Dictionary[connection.Id].Connection.State ||
                        Dictionary[connection.Id].JustProcessed
                        )
                    {
                        NetworkItem oldConnection = Dictionary[connection.Id].Clone() as NetworkItem;

                        newDict[connection.Id].Connection.State = connection.Connection.State;
                        newDict[connection.Id].JustProcessed = false;

                        OnDictionaryModified(oldConnection, newDict[connection.Id]);
                    }
                }
            }

            this.Dictionary = newDict;
        }

        private void ResolveAddresses(string id, bool remote, IPAddress address)
        {
            string hostName = null;
            bool inCache = false;


            lock (_resolveCache)
            {
                if (_resolveCache.ContainsKey(address))
                {
                    hostName = _resolveCache[address];
                    inCache = true;
                }
            }


            if (!inCache)
            {
                try
                {
                    hostName = Dns.GetHostEntry(address).HostName;
                }
                catch (SocketException)
                {

                    return;
                }


                lock (_resolveCache)
                {

                    if (!string.IsNullOrEmpty(hostName))
                    {
                        if (!_resolveCache.ContainsKey(address))
                            _resolveCache.Add(address, hostName);
                    }
                }
            }

            _messageQueue.Enqueue(new AddressResolveMessage()
            {
                Id = id,
                Remote = remote,
                HostName = hostName
            });
        }
    }
}
