

using System;
using System.Collections.Generic;
using ProcessHacker.Native;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker
{
    public class ServiceItem : ICloneable
    {
        public object Clone()
        {
            return this.MemberwiseClone();
        }

        public int RunId;
        public EnumServiceStatusProcess Status;
        public QueryServiceConfig Config;
    }

    public class ServiceProvider : Provider<string, ServiceItem>
    {
        public ServiceProvider()
            : base(StringComparer.InvariantCultureIgnoreCase)
        {
            this.Name = this.GetType().Name;
            this.ProviderUpdate += new ProviderUpdateOnce(UpdateOnce);
        }

        public void UpdateServiceConfig(string name, QueryServiceConfig config)
        {
            ServiceItem item = Dictionary[name];

            Dictionary[name] = new ServiceItem()
            {
                Config = config,
                Status = item.Status
            };

            this.OnDictionaryModified(item, Dictionary[name]);
        }

        private void UpdateOnce()
        {
            var newdictionary = Windows.GetServices();


            foreach (string s in Dictionary.Keys)
            {
                if (!newdictionary.ContainsKey(s))
                {
                    ServiceItem service = Dictionary[s];

                    this.OnDictionaryRemoved(service);
                    Dictionary.Remove(s);
                }
            }


            foreach (string s in newdictionary.Keys)
            {
                if (!Dictionary.ContainsKey(s))
                {
                    ServiceItem item = new ServiceItem();

                    item.RunId = this.RunCount;
                    item.Status = newdictionary[s];

                    try
                    {
                        using (var shandle = new ServiceHandle(s, ServiceAccess.QueryConfig))
                            item.Config = shandle.GetConfig();
                    }
                    catch
                    { }

                    this.OnDictionaryAdded(item);
                    Dictionary.Add(s, item);
                }
            }

            var toModify = new Dictionary<string, ServiceItem>();


            foreach (ServiceItem service in Dictionary.Values)
            {
                var newStatus = newdictionary[service.Status.ServiceName];

                bool modified = false;

                if (service.Status.DisplayName != newStatus.DisplayName)
                    modified = true;
                else if (service.Status.ServiceStatusProcess.ControlsAccepted !=
                    newStatus.ServiceStatusProcess.ControlsAccepted)
                    modified = true;
                else if (service.Status.ServiceStatusProcess.CurrentState !=
                    newStatus.ServiceStatusProcess.CurrentState)
                    modified = true;
                else if (service.Status.ServiceStatusProcess.ProcessID !=
                    newStatus.ServiceStatusProcess.ProcessID)
                    modified = true;
                else if (service.Status.ServiceStatusProcess.ServiceFlags !=
                    newStatus.ServiceStatusProcess.ServiceFlags)
                    modified = true;
                else if (service.Status.ServiceStatusProcess.ServiceType !=
                    newStatus.ServiceStatusProcess.ServiceType)
                    modified = true;

                if (modified)
                {
                    var newServiceItem = new ServiceItem()
                    {
                        RunId = service.RunId,
                        Status = newStatus,
                        Config = service.Config
                    };

                    this.OnDictionaryModified(service, newServiceItem);
                    toModify.Add(service.Status.ServiceName, newServiceItem);
                }
            }

            foreach (string serviceName in toModify.Keys)
                Dictionary[serviceName] = toModify[serviceName];
        }
    }
}
