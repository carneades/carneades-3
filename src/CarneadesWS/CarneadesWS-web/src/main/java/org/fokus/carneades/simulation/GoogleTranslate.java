/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.fokus.carneades.simulation;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIUtils;
import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONObject;

/**
 *
 * @author stb
 */
public class GoogleTranslate {
    
    // TODO : get key for impact
    private static final String translateKey = "AIzaSyCu5HOkAZ-1d196HCu4iOHStjrmhZ8Ne48";
    
    private static final String translateScheme = "https";
    private static final String translateHost = "www.googleapis.com";
    private static final String translatePath = "/language/translate/v2";
    
    public static String translate(String text, String sourceLang, String targetLang) {
        
        String t = "";
        
        try {
        
            List<NameValuePair> qparams = new ArrayList<NameValuePair>();
            qparams.add(new BasicNameValuePair("key", translateKey));
            qparams.add(new BasicNameValuePair("q", text));
            qparams.add(new BasicNameValuePair("source", sourceLang));
            qparams.add(new BasicNameValuePair("target", targetLang));
            

            URI uri = URIUtils.createURI(translateScheme, translateHost, -1, translatePath, URLEncodedUtils.format(qparams, "UTF-8"), null);            
            HttpClient httpclient = new DefaultHttpClient();
            HttpGet httpget = new HttpGet(uri);
            System.out.println(httpget.getURI());
            HttpResponse response = httpclient.execute(httpget);
            
            HttpEntity entity = response.getEntity();
            
            //System.out.println(EntityUtils.toString(entity));
            JSONObject jsonResponse = new JSONObject(EntityUtils.toString(entity));
            JSONObject jsonData = jsonResponse.getJSONObject("data");
            JSONArray jsonTranslations = jsonData.getJSONArray("translations");
            JSONObject jsonLang = jsonTranslations.getJSONObject(0);
            t = jsonLang.getString("translatedText");
            
            
        } catch(Exception e) {
            e.printStackTrace();
        } finally {
            return t;
        }
        
    }
    
    public static List<String> getLanguages() {
        
        List<String> languages = new ArrayList<String>();
        
        try {
            List<NameValuePair> qparams = new ArrayList<NameValuePair>();
            qparams.add(new BasicNameValuePair("key", translateKey));
            String path = translatePath + "/languages";

            URI uri = URIUtils.createURI(translateScheme, translateHost, -1, path, URLEncodedUtils.format(qparams, "UTF-8"), null);            
            HttpClient httpclient = new DefaultHttpClient();
            HttpGet httpget = new HttpGet(uri);
            System.out.println(httpget.getURI());
            HttpResponse response = httpclient.execute(httpget);
            
            HttpEntity entity = response.getEntity();
            
            JSONObject jsonResponse = new JSONObject(EntityUtils.toString(entity));
            
            JSONObject jsonData = jsonResponse.getJSONObject("data");
            JSONArray jsonLanguages = jsonData.getJSONArray("languages");
            
            for(int i=0; i<jsonLanguages.length(); i++) {
                JSONObject jsonLang = jsonLanguages.getJSONObject(i);
                languages.add(jsonLang.getString("language"));
            }
        
        } catch(Exception e) {
            e.printStackTrace();
        } finally {
            return languages;
        }
        
        
    }
    
}
