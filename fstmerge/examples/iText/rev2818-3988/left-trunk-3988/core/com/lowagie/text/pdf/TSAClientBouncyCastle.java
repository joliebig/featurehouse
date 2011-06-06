

package com.lowagie.text.pdf;

import java.io.*;
import java.math.*;
import java.net.*;

import org.bouncycastle.asn1.cmp.*;
import org.bouncycastle.asn1.x509.*;
import org.bouncycastle.tsp.*;

import com.lowagie.text.pdf.codec.Base64;


public class TSAClientBouncyCastle implements TSAClient {
    
    protected String tsaURL;
    
    protected String tsaUsername;
    
    protected String tsaPassword;
    
    protected int tokSzEstimate;
    
    
    public TSAClientBouncyCastle(String url) {
        this(url, null, null, 4096);
    }
    
    
    public TSAClientBouncyCastle(String url, String username, String password) {
        this(url, username, password, 4096);
    }
    
    
    public TSAClientBouncyCastle(String url, String username, String password, int tokSzEstimate) {
        this.tsaURL       = url;
        this.tsaUsername  = username;
        this.tsaPassword  = password;
        this.tokSzEstimate = tokSzEstimate;
    }
    
    
    public int getTokenSizeEstimate() {
        return tokSzEstimate;
    }
    
    
    public byte[] getTimeStampToken(PdfPKCS7 caller, byte[] imprint) throws Exception {
        return getTimeStampToken(imprint);
    }
    
    
    protected byte[] getTimeStampToken(byte[] imprint) throws Exception {
        byte[] respBytes = null;
        try {
            
            TimeStampRequestGenerator tsqGenerator = new TimeStampRequestGenerator();
            tsqGenerator.setCertReq(true);
            
            BigInteger nonce = BigInteger.valueOf(System.currentTimeMillis());
            TimeStampRequest request = tsqGenerator.generate(X509ObjectIdentifiers.id_SHA1.getId() , imprint, nonce);
            byte[] requestBytes = request.getEncoded();
            
            
            respBytes = getTSAResponse(requestBytes);
            
            
            TimeStampResponse response = new TimeStampResponse(respBytes);
            
            
            response.validate(request);
            PKIFailureInfo failure = response.getFailInfo();
            int value = (failure == null) ? 0 : failure.intValue();
            if (value != 0) {
                
                throw new Exception("Invalid TSA '" + tsaURL + "' response, code " + value);
            }
            
            
            
            
            TimeStampToken  tsToken = response.getTimeStampToken();
            if (tsToken == null) {
                throw new Exception("TSA '" + tsaURL + "' failed to return time stamp token: " + response.getStatusString());
            }
            TimeStampTokenInfo info = tsToken.getTimeStampInfo(); 
            byte[] encoded = tsToken.getEncoded();
            long stop = System.currentTimeMillis();
            
            
            this.tokSzEstimate = encoded.length + 32;
            return encoded;
        } catch (Exception e) {
            throw e;
        } catch (Throwable t) {
            throw new Exception("Failed to get TSA response from '" + tsaURL +"'", t);
        }
    }
    
    
    protected byte[] getTSAResponse(byte[] requestBytes) throws Exception {
        
        URL url = new URL(tsaURL);
        URLConnection tsaConnection;
        tsaConnection = (URLConnection) url.openConnection();
        
        tsaConnection.setDoInput(true);
        tsaConnection.setDoOutput(true);
        tsaConnection.setUseCaches(false);
        tsaConnection.setRequestProperty("Content-Type", "application/timestamp-query");
        
        tsaConnection.setRequestProperty("Content-Transfer-Encoding", "binary");
        
        if ((tsaUsername != null) && !tsaUsername.equals("") ) {
            String userPassword = tsaUsername + ":" + tsaPassword;
            tsaConnection.setRequestProperty("Authorization", "Basic " +
                new String(Base64.encodeBytes(userPassword.getBytes())));
        }
        OutputStream out = tsaConnection.getOutputStream();
        out.write(requestBytes);
        out.close();
        
        
        InputStream inp = tsaConnection.getInputStream();
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte[] buffer = new byte[1024];
        int bytesRead = 0;
        while ((bytesRead = inp.read(buffer, 0, buffer.length)) >= 0) {
            baos.write(buffer, 0, bytesRead);
        }
        byte[] respBytes = baos.toByteArray();
        
        String encoding = tsaConnection.getContentEncoding();
        if (encoding != null && encoding.equalsIgnoreCase("base64")) {
            respBytes = Base64.decode(new String(respBytes));
        }
        return respBytes;
    }    
}