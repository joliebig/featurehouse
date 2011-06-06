
package org.gjt.sp.util;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.util.Properties;
import java.util.StringTokenizer;


public abstract class PropertiesBean
{

	

	
	protected PropertiesBean(String root)
	{
		this(root, ':');
	}

	
	protected PropertiesBean(String root, char arraysep)
	{
		if (root == null)
			throw new IllegalArgumentException("root cannot be null");
		this.root = root;
		this.arraysep = arraysep;
	}

	

	
	public void load(Properties p)
	{
		try
		{
			PropertyDescriptor[] _props = getPropertyDescriptors();
			for (int i = 0; i < _props.length; i++)
			{
				if ("class".equals(_props[i].getName()))
					continue;

				Method _set = _props[i].getWriteMethod();
				if (_set != null)
				{
					String _pname = root + "." + _props[i].getName();
					Object _val = p.getProperty(_pname);
					if (_val != null)
						_val = parse((String)_val, _props[i].getPropertyType());
					try
					{
						_set.invoke(this, _val);
					}
					catch (IllegalArgumentException iae)
					{
						
					}
				}
			}
		}
		catch (Exception e)
		{
			
			
			
			Log.log(Log.ERROR, this, e);
		}
	}

	
	public void save(Properties p)
	{
		try
		{
			PropertyDescriptor[] _props = getPropertyDescriptors();
			for (int i = 0; i < _props.length; i++)
			{
				if ("class".equals(_props[i].getName()))
					continue;

				Method _get = _props[i].getReadMethod();
				if (_get != null)
				{
					Object _val = _get.invoke(this);
					String _pname = root + "." + _props[i].getName();
					if (_val != null)
						p.setProperty(_pname, encode(_val));
					else
						p.remove(_pname);
				}
			}
		}
		catch (Exception e)
		{
			
			
			
			Log.log(Log.ERROR, this, e);
		}
	}

	
	public void clean(Properties p)
	{

		try
		{
			PropertyDescriptor[] _props = getPropertyDescriptors();
			for (int i = 0; i < _props.length; i++)
			{
				if ("class".equals(_props[i].getName()))
					continue;

				String _pname = root + "." + _props[i].getName();
				p.remove(_pname);
			}
		}
		catch (Exception e)
		{
			
			
			
			Log.log(Log.ERROR, this, e);
		}
	}

	

	private PropertyDescriptor[] getPropertyDescriptors()
		throws IntrospectionException
	{
		BeanInfo _info = Introspector.getBeanInfo(getClass());
		return _info.getPropertyDescriptors();
	}

	private String encode(Object value)
	{
		Class _class = value.getClass();
		if (_class.isArray())
		{
			StringBuilder _val = new StringBuilder();
			int _len = Array.getLength(value);
			for (int i = 0; i < _len; i++)
			{
				String _str = encode(Array.get(value, i));
				if (_str == null)
					return null;
				_val.append(_str);
				if (i < _len - 1)
					_val.append(arraysep);
			}
			return _val.toString();
		}
		else
		{
			
			if (_class != Boolean.class && _class != Boolean.TYPE
			    && _class != Character.class && _class != Character.TYPE
			    && _class != Double.class && _class != Double.TYPE
			    && _class != Float.class && _class != Float.TYPE
			    && _class != Integer.class && _class != Integer.TYPE
			    && _class != Long.class && _class != Long.TYPE
			    && _class != Short.class && _class != Short.TYPE
			    && _class != String.class)
			{
				Log.log(Log.WARNING, this, "unsupported type: " + _class.getName());
				return null;
			}
			return value.toString();
		}
	}

	private Object parse(String value, Class<?> _class)
	{
		Object _ret = null;
		if (_class.isArray())
		{
			StringTokenizer st = new StringTokenizer(value, String.valueOf(arraysep));
			Class _type = _class.getComponentType();
			_ret = Array.newInstance(_type, st.countTokens());
			int _cnt = st.countTokens();
			for (int i = 0; i < _cnt; i++)
			{
				Object _val = parse(st.nextToken(), _type);
				if (_val == null)
					return null;
				Array.set(_ret, i, _val);
			}
		}
		else
		{
			if (_class == Boolean.class || _class == Boolean.TYPE)
				_ret = Boolean.valueOf(value);
			else if (_class == Character.class || _class == Character.TYPE)
				_ret = Character.valueOf(value.charAt(0));
			else if (_class == Double.class || _class == Double.TYPE)
				_ret = Double.valueOf(value);
			else if (_class == Float.class || _class == Float.TYPE)
				_ret = Float.valueOf(value);
			else if (_class == Integer.class || _class == Integer.TYPE)
				_ret = Integer.valueOf(value);
			else if (_class == Long.class || _class == Long.TYPE)
				_ret = Long.valueOf(value);
			else if (_class == Short.class || _class == Short.TYPE)
				_ret = Short.valueOf(value);
			else if (_class == String.class)
				_ret = value;
			else
				Log.log(Log.WARNING, this, "unsupported type: " + _class.getName());

		}
		return _ret;
	}

	

	private final char		arraysep;
	private final String 	root;

}

