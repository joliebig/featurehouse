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
using System.IO;
using System.Reflection;
using System.Xml.Serialization;
using System.Text;
using System.Text.RegularExpressions;
using System.Windows.Forms;

using MeGUI.core.gui;
using MeGUI.core.plugins.interfaces;
using MeGUI.core.util;
using MeGUI.packages.audio.faac;
using MeGUI.packages.tools.oneclick;
using MeGUI.packages.video.x264;

namespace MeGUI
{
    public interface Editable<TSettings>
    {
        TSettings Settings
        {
            get;
            set;
        }
    }

    public class ProfileManager
    {
        
        public ProfileManager(string path)
        {
            this.path = path;
            profileGroups.Add(new ProfileGroup(typeof(VideoCodecSettings), "Video"));
            SafeRegister<x264Settings, x264ConfigurationPanel>("Video");
            SafeRegister<xvidSettings, MeGUI.packages.video.xvid.xvidConfigurationPanel>("Video");
            SafeRegister<snowSettings, MeGUI.packages.video.snow.snowConfigurationPanel>("Video");
            profileGroups.Add(new ProfileGroup(typeof(AudioCodecSettings), "Audio"));
            SafeRegister<AftenSettings, MeGUI.packages.audio.aften.AftenConfigurationPanel>("Audio");
            SafeRegister<AudXSettings, MeGUI.packages.audio.audx.AudXConfigurationPanel>("Audio");
            SafeRegister<FaacSettings, faacConfigurationPanel>("Audio");
            SafeRegister<AC3Settings, MeGUI.packages.audio.ffac3.AC3ConfigurationPanel>("Audio");
            SafeRegister<MP2Settings, MeGUI.packages.audio.ffmp2.MP2ConfigurationPanel>("Audio");
            SafeRegister<MP3Settings, MeGUI.packages.audio.lame.lameConfigurationPanel>("Audio");
            SafeRegister<NeroAACSettings, MeGUI.packages.audio.naac.neroConfigurationPanel>("Audio");
            SafeRegister<OggVorbisSettings, MeGUI.packages.audio.vorbis.OggVorbisConfigurationPanel>("Audio");
            SafeRegister<WinAmpAACSettings, MeGUI.packages.audio.waac.WinAmpAACConfigurationPanel>("Audio");
            
            SafeRegister<OneClickSettings, OneClickConfigPanel>();
            SafeRegister<AviSynthSettings, AviSynthProfileConfigPanel>();
        }
        

        public static readonly string ScratchPadName = "*scratchpad*";

        string path;
        private List<ProfileType> profileTypes = new List<ProfileType>();
        private List<ProfileGroup> profileGroups = new List<ProfileGroup>();



        
        public bool SafeRegister<TSettings, TPanel>(params string[] groups)
            where TSettings : GenericSettings, new()
            where TPanel : Control, Editable<TSettings>, new()
        {
            string name = new TSettings().SettingsID;
            if (byName(name) != null) return false;
            if (bySettingsType(typeof(TSettings)) != null) return false;

            ProfileType p = new SpecificProfileType<TSettings, TPanel>(name);
            profileTypes.Add(p);

            foreach (string g in groups)
                groupByName(g).Register(p, name, typeof(TSettings));
            return true;
        }

        public bool Register(Type TSettings, Type TPanel, string name)
        {
            MethodInfo m = typeof(ProfileManager).GetMethod("SafeRegister");
            MethodInfo m2 = m.MakeGenericMethod(TSettings, TPanel);
            return (bool)m2.Invoke(this, new object[] { name });
        }
        

        
        /// <summary>
        /// eliminates non allowed characters and replaced them with an underscore
        /// </summary>
        /// <param name="name"></param>
        /// <returns></returns>
        private static string fixName(string name)
        {
            name = name.Replace("\"", "_hc_");
            name = name.Replace("*", "_st_");
            name = name.Replace("/", "_sl_");
            name = name.Replace(":", "_dp_");
            name = name.Replace("<", "_lt_");
            name = name.Replace(">", "_rt_");
            name = name.Replace("?", "_qm_");
            name = name.Replace("\\", "_bs_");
            name = name.Replace("|", "_vl_");
            return name;
        }

        private ProfileGroup groupByName(string name)
        {
            return Util.ByID(profileGroups, name);
        }
        
        private ProfileType byName(string name)
        {
            return Util.ByID(profileTypes, name);
        }

        private ProfileType bySettingsType(Type t)
        {
            return profileTypes.Find(delegate(ProfileType p) { return p.SettingsType == t; });
        }

        private Type profileType(string id)
        {
            return typeof(GenericProfile<>).MakeGenericType(byName(id).SettingsType);
        }
        

        
        private static string profilePath(string path, Profile prof)
        {
            return Path.Combine(Path.Combine(GetSaveFolder(path), prof.BaseSettings.SettingsID), fixName(prof.FQName) + ".xml");
        }

        const string SaveFolderName = "allprofiles";

        public static string GetSaveFolder(string path)
        {
            return Path.Combine(path, SaveFolderName);
        }

        /// <summary>
        /// saves all the profiles
        /// this is called when the program exists and ensures that all
        /// currently defined profiles are saved, overwriting currently existing ones
        /// </summary>
        public static void WriteProfiles(string path, IEnumerable<Profile> profiles)
        {
            try
            {
                FileUtil.DeleteDirectoryIfExists(GetSaveFolder(path), true);
            }
            catch (Exception ex)
            {
                MessageBox.Show("There was an error clearing the profiles folder before deletion: " + ex.Message, "Error saving profiles", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

            foreach (Profile p in profiles)
                Util.XmlSerialize(p, profilePath(path, p));
        }

        public void SaveProfiles()
        {
            WriteProfiles(path, AllProfiles);
            saveSelectedProfiles();
        }

        private void saveSelectedProfiles()
        {
            string filename = Path.Combine(GetSaveFolder(path), "SelectedProfiles.xml");
            XmlSerializer ser = null;
            using (Stream s = File.Open(filename, System.IO.FileMode.Create, System.IO.FileAccess.Write))
            {
                try
                {
                    // Hacky workaround because serialization of dictionaries isn't possible
                    string[] groupKeys = profileGroups.ConvertAll<string>(delegate(ProfileGroup g) { return g.ID; }).ToArray();
                    string[] groupValues = profileGroups.ConvertAll<string>(delegate(ProfileGroup g) { return g.SelectedChild; }).ToArray();
                    string[] keys = profileTypes.ConvertAll<string>(delegate(ProfileType p) { return p.ID; }).ToArray();
                    string[] values = profileTypes.ConvertAll<string>(delegate(ProfileType p) { return p.SelectedProfile.Name; }).ToArray();
                    string[][] data = new string[][] { keys, values, groupKeys, groupValues };
                    ser = new XmlSerializer(data.GetType());
                    ser.Serialize(s, data);
                }
                catch (Exception e)
                {
                    MessageBox.Show("List of selected profiles could not be saved. Error message: " + e.Message, "Error saving profile", MessageBoxButtons.OK);
                }
            }
        }

        private void loadSelectedProfiles()
        {
            string file = Path.Combine(GetSaveFolder(path), "SelectedProfiles.xml");
            if (!File.Exists(file)) return;
            using (Stream s = File.OpenRead(file))
            {
                try
                {
                    // Hacky workaround because serialization of dictionaries isn't possible
                    XmlSerializer ser = new XmlSerializer(typeof(string[][]));
                    string[][] data = (string[][])ser.Deserialize(s);
                    string[] keys = data[0];
                    string[] values = data[1];
                    string[] groupKeys = data[2];
                    string[] groupValues = data[3];
                    System.Diagnostics.Debug.Assert(keys.Length == values.Length);
                    Debug.Assert(groupKeys.Length == groupValues.Length);
                    for (int i = 0; i < keys.Length; i++)
                    {
                        byName(keys[i]).SelectedProfile = byName(keys[i]).ByName(values[i]);
                    }

                    for (int i = 0; i < groupKeys.Length; ++i)
                    {
                        groupByName(groupKeys[i]).SelectedChild = groupValues[i];
                    }
                }
                catch (Exception)
                {
                    MessageBox.Show("List of selected profiles could not be loaded.", "Error loading profile", MessageBoxButtons.OK);
                }
            }
        }

        public void LoadProfiles()
        {
            if (Directory.Exists(Path.Combine(path, "profiles")))
            {
                //setAllProfiles(Loader.TryLoadProfiles(path));
                FileUtil.DeleteDirectoryIfExists(Path.Combine(path, "profiles"), true);
                return;
            }

            foreach (ProfileType t in profileTypes)
                t.Profiles = readAllProfiles(t);

            loadSelectedProfiles();
        }

        public static List<Profile> ReadAllProfiles(string path)
        {
            ProfileManager p = new ProfileManager(path);
            //if (Directory.Exists(Path.Combine(path, "profiles")))
            //    return Loader.TryLoadProfiles(path);

            List<Profile> ps = new List<Profile>();

            foreach (ProfileType t in p.profileTypes)
                ps.AddRange(p.readAllProfiles(t));

            return ps;
        }

        private List<Profile> readAllProfiles(ProfileType type)
        {
            string profilePath = Path.Combine(GetSaveFolder(path), type.ID);
            DirectoryInfo di = FileUtil.ensureDirectoryExists(profilePath);
            FileInfo[] files = di.GetFiles("*.xml");

            return Util.RemoveDuds(new List<FileInfo>(files).ConvertAll<Profile>(
                delegate(FileInfo fi)
                {
                    return (Profile)Util.XmlDeserialize(fi.FullName, type.GenericProfileType);
                }));
        }

        private void setAllProfiles(List<Profile> ps)
        {
            ps = Util.RemoveDuds(ps);
            foreach (ProfileType type in profileTypes)
            {
                type.Profiles = ps.FindAll(
                    delegate(Profile p) { return p.GetType().Equals(type.GenericProfileType); });
            }
        }
        

        
        public Profile GetSelectedProfile(string profileType)
        {
            ProfileType t = byName(profileType);
            if (t != null)
                return t.SelectedProfile;

            ProfileGroup g = groupByName(profileType);
            Debug.Assert(g != null);

            return GetSelectedProfile(g.SelectedChild);
        }

        public void SetSelectedProfile(Profile prof)
        {
            bySettingsType(GetSettingsType(prof)).SelectedProfile = prof;
        }
        

        public Profile GetProfile(string type, string name)
        {
            return byName(type).ByName(name);
        }

        
        private List<ProfileType> byProfileSet(string profileSet)
        {
            List<ProfileType> res = new List<ProfileType>();
            ProfileType p = byName(profileSet);
            if (p != null)
            {
                res.Add(p);
                return res;
            }
            
            ProfileGroup g = groupByName(profileSet);
            if (g == null)
                throw new Exception();

            foreach (string s in g.ChildIDs)
                res.AddRange(byProfileSet(s));

            return res;
        }

        public IEnumerable<Named<Profile> > Profiles(string type) 
        {
            List<ProfileType> ps = byProfileSet(type);
            
            if (ps.Count == 1)
                return Util.ConvertAll<Profile, Named<Profile> >(ps[0].Profiles, delegate (Profile pr) { return new Named<Profile>(pr.Name, pr); });

            List<Named<Profile> > profiles = new List<Named<Profile> >();
            foreach (ProfileType p in ps)
                profiles.AddRange(Util.ConvertAll<Profile, Named<Profile> >(p.Profiles, delegate (Profile prof) { return new Named<Profile>(prof.FQName, prof); }));

            return profiles;
        }

        public void AddProfilesChangedListener(string profileSet, EventHandler listener)
        {
            foreach (ProfileType p in byProfileSet(profileSet))
                p.ProfilesChanged += listener;
        }

        public void RemoveProfilesChangedListener(string profileSet, EventHandler listener)
        {
            foreach (ProfileType p in byProfileSet(profileSet))
                p.ProfilesChanged -= listener;
        }
        

        
        private Profile byFormattedName(string formattedName)
        {
            string type = formattedName.Substring(0, formattedName.IndexOf(':'));
            System.Diagnostics.Debug.Assert(formattedName.StartsWith(type + ": "));
            string profileName = formattedName.Substring(type.Length + 2);
            return GetProfile(type, profileName);
        }

        public Profile[] AllProfiles
        {
            get
            {
                List<Profile> profileList = new List<Profile>();
                
                foreach (ProfileType ty in profileTypes)
                    profileList.AddRange(ty.Profiles);
                
                return profileList.ToArray();
            }
        }

        public void AddAll(Profile[] profiles, DialogManager asker)
        {
            foreach (Profile prof in profiles)
            {
                bySettingsType(GetSettingsType(prof)).Add(prof, asker);
            }
        }
        


        
        private static Type GetSettingsType(Profile p)
        {
            return p.GetType().GetGenericArguments()[0];
        }

        public void Configure(Profile SelectedProfile)
        {
            bySettingsType(GetSettingsType(SelectedProfile)).ConfigureProfiles();
        }

        public void SetSettings(GenericSettings s)
        {
            bySettingsType(s.GetType()).SetSettings(s);
        }

        public GenericSettings GetCurrentSettings(string p)
        {
            return GetSelectedProfile(p).BaseSettings;
        }
        
    }

    abstract class ProfileType : IIDable
    {
        public event EventHandler SelectedProfileSet;
        public event EventHandler ProfilesChanged;

        private readonly string profileTypeID;

        public string ID { get { return profileTypeID; } }

        public Tuple<IEnumerable<Profile>, Profile> ProfilesAndSelected
        {
            get { return new Tuple<IEnumerable<Profile>, Profile>(profiles, selectedProfile); }
            set
            {
                profiles = new List<Profile>(value.a);
                if (profiles.Count == 0)
                    profiles.Add(genScratchpad());

                selectedProfile = value.b ?? profiles[0];

                raiseChangedEvent();
            }
        }

        public IEnumerable<Profile> Profiles
        {
            get { return ProfilesAndSelected.a; }
            set { ProfilesAndSelected = new Tuple<IEnumerable<Profile>,Profile>(value, null); }
        }

        private void raiseChangedEvent()
        {
            if (ProfilesChanged != null)
                ProfilesChanged(this, EventArgs.Empty);
        }

        protected List<Profile> profiles = new List<Profile>();

        protected abstract Profile genScratchpad();

        private Profile selectedProfile;
        public Profile SelectedProfile
        {
            get { return selectedProfile; }
            set
            {
                selectedProfile = value ?? selectedProfile;
                if (SelectedProfileSet != null)
                    SelectedProfileSet(this, EventArgs.Empty);
            }
        }

        public void SetSettings(GenericSettings s)
        {
            if (s.GetType() != SettingsType)
                throw new Exception("Wrong type of settings used");

            Profile p = ByName(ProfileManager.ScratchPadName);
            if (p == null)
            {
                p = genScratchpad();
                profiles.Add(p);
            }
            p.BaseSettings = s;
            SelectedProfile = p;
        }

        public abstract void ConfigureProfiles();

        public abstract Type SettingsType { get; }
        
        public Type GenericProfileType
        {
            get { return typeof(GenericProfile<>).MakeGenericType(SettingsType); }
        }

        public ProfileType(string name)
        {
            profileTypeID = name;
        }

        public Profile ByName(string name)
        {
            int i = indexOf(name);
            if (i >= 0)
                return profiles[i];
            else
                return null;
        }

        private int indexOf(string name)
        {
            return profiles.FindIndex(delegate(Profile p) { return p.Name == name; });
        }

        public void Add(Profile prof, DialogManager asker)
        {
            int i = indexOf(prof.Name);
            if (i < 0)
            {
                profiles.Add(prof);
                raiseChangedEvent();
            }
            else if (asker.overwriteProfile(prof.FQName))
                profiles[i] = prof;
            else { /* skip this profile */ }
        }
    }

    abstract class SpecificProfileType<TSettings> : ProfileType
        where TSettings : GenericSettings, new()
    {
        public SpecificProfileType(string name) : base(name) { }

        public override Type SettingsType
        {
            get { return typeof(TSettings); }
        }

        public GenericProfile<TSettings> SByName(string name)
        {
            return (GenericProfile<TSettings>)ByName(name);
        }

        public Tuple<IEnumerable<GenericProfile<TSettings> >, GenericProfile<TSettings> > SProfiles
        {
            get
            {
                Tuple<IEnumerable<Profile>, Profile> p = ProfilesAndSelected;

                return new Tuple<IEnumerable<GenericProfile<TSettings> >, GenericProfile<TSettings> >(
                    Util.CastAll<Profile, GenericProfile<TSettings> >(p.a),
                    (GenericProfile<TSettings>)p.b);
            }
            set
            {
                ProfilesAndSelected = new Tuple<IEnumerable<Profile>, Profile>(
                    Util.CastAll<GenericProfile<TSettings>, Profile>(value.a),
                    value.b);
            }
        }

        public GenericProfile<TSettings> SSelectedProfile
        {
            get { return (GenericProfile<TSettings>)SelectedProfile; }
            set { SelectedProfile = value; }
        }

        protected override Profile genScratchpad()
        {
            return new GenericProfile<TSettings>(ProfileManager.ScratchPadName, new TSettings());
        }
    }

    class SpecificProfileType<TSettings, TPanel> : SpecificProfileType<TSettings>
        where TSettings : GenericSettings, new()
        where TPanel : Control, Editable<TSettings>, new()
    {
        public override void ConfigureProfiles()
        {
            // used to init with mainForm, but that is not required any more. OTOH, a ProfileManager instance might be required
            TPanel t = new TPanel();
            ProfileConfigurationWindow<TSettings, TPanel> w = new ProfileConfigurationWindow<TSettings, TPanel>(t, ID);
            w.Profiles = SProfiles;
            if (w.ShowDialog() == DialogResult.Cancel)
                return;

            SProfiles = w.Profiles;
        }

        public SpecificProfileType(string name)
            : base(name)
        { }
    }

    class ProfileGroup : IIDable
    {
        private readonly string profileGroupID;

        public string ID { get { return profileGroupID; } }

        public List<string> ChildIDs = new List<string>();
        public readonly Type CommonType;
        public string SelectedChild = null;

        public ProfileGroup(Type t, string name)
        {
            Debug.Assert(Array.IndexOf(t.GetInterfaces(), typeof(GenericSettings)) >= 0);
            //Debug.Assert(t.IsSubclassOf(typeof(GenericSettings)));
            CommonType = t;
            profileGroupID = name;
        }

        public void Register(ProfileType t, string childID, Type childType)
        {
            Debug.Assert(childType.IsSubclassOf(CommonType));

            if (!ChildIDs.Contains(childID))
                ChildIDs.Add(childID);

            if (SelectedChild == null)
                SelectedChild = childID;

            t.SelectedProfileSet += delegate(object _, EventArgs __)
            {
                SelectedChild = childID;
            };
        }
    }

}
