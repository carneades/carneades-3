/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.fokus.carneades.simulation;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author stb
 */
public class Languages {
    
    private Map<String, String> langRepresentations;
    private Map<String, String> langValues;

    public Languages() {  
        
        langRepresentations = new HashMap<String, String>();
        langRepresentations.put("af", "Afrikaans");
        langRepresentations.put("sq", "Albanian");
        langRepresentations.put("ar", "Arabic");
        langRepresentations.put("be", "Belarusian");
        langRepresentations.put("bg", "Bulgarian");
        langRepresentations.put("ca", "Catalan");
        langRepresentations.put("zh-CN", "Chinese Simplified");
        langRepresentations.put("zh-TW", "Chinese Traditional");
        langRepresentations.put("hr", "Croatian");
        langRepresentations.put("cs", "Czech");
        langRepresentations.put("da", "Danish");
        langRepresentations.put("nl", "Dutch");
        langRepresentations.put("en", "English");
        langRepresentations.put("et", "Estonian");
        langRepresentations.put("tl", "Filipino");
        langRepresentations.put("fi", "Finnish");
        langRepresentations.put("fr", "French");
        langRepresentations.put("gl", "Galician");
        langRepresentations.put("de", "German");
        langRepresentations.put("el", "Greek");
        langRepresentations.put("ht", "Haitian Creole");
        langRepresentations.put("iw", "Hebrew");
        langRepresentations.put("hi", "Hindi");
        langRepresentations.put("hu", "Hungarian");
        langRepresentations.put("is", "Icelandic");
        langRepresentations.put("id", "Indonesian");
        langRepresentations.put("ga", "Irish");
        langRepresentations.put("it", "Italian");
        langRepresentations.put("ja", "Japanese");
        langRepresentations.put("lv", "Latvian");
        langRepresentations.put("lt", "Lithuanian");
        langRepresentations.put("mk", "Macedonian");
        langRepresentations.put("ms", "Malay");
        langRepresentations.put("mt", "Maltese");
        langRepresentations.put("no", "Norwegian");
        langRepresentations.put("fa", "Persian");
        langRepresentations.put("pl", "Polish");
        langRepresentations.put("pt", "Portuguese");
        langRepresentations.put("ro", "Romanian");
        langRepresentations.put("ru", "Russian");
        langRepresentations.put("sr", "Serbian");
        langRepresentations.put("sk", "Slovak");
        langRepresentations.put("sl", "Slovenian");
        langRepresentations.put("es", "Spanish");
        langRepresentations.put("sw", "Swahili");
        langRepresentations.put("sv", "Swedish");
        langRepresentations.put("th", "Thai");
        langRepresentations.put("tr", "Turkish");
        langRepresentations.put("uk", "Ukrainian");
        langRepresentations.put("vi", "Vietnamese");
        langRepresentations.put("cy", "Welsh");
        langRepresentations.put("yi", "Yiddish");        
        
        langValues = new HashMap<String, String>();
        langValues.put("Afrikaans", "af");
        langValues.put("Albanian", "sq");
        langValues.put("Arabic", "ar");
        langValues.put("Belarusian", "be");
        langValues.put("Bulgarian", "bg");
        langValues.put("Catalan", "ca");
        langValues.put("Chinese Simplified", "zh-CN");
        langValues.put("Chinese Traditional", "zh-TW");
        langValues.put("Croatian", "hr");
        langValues.put("Czech", "cs");
        langValues.put("Danish", "da");
        langValues.put("Dutch", "nl");
        langValues.put("English", "en");
        langValues.put("Estonian", "et");
        langValues.put("Filipino", "tl");
        langValues.put("Finnish", "fi");
        langValues.put("French", "fr");
        langValues.put("Galician", "gl");
        langValues.put("German", "de");
        langValues.put("Greek", "el");
        langValues.put("Haitian Creole", "ht");
        langValues.put("Hebrew", "iw");
        langValues.put("Hindi", "hi");
        langValues.put("Hungarian", "hu");
        langValues.put("Icelandic", "is");
        langValues.put("Indonesian", "id");
        langValues.put("Irish", "ga");
        langValues.put("Italian", "it");
        langValues.put("Japanese", "ja");
        langValues.put("Latvian", "lv");
        langValues.put("Lithuanian", "lt");
        langValues.put("Macedonian", "mk");
        langValues.put("Malay", "ms");
        langValues.put("Maltese", "mt");
        langValues.put("Norwegian", "no");
        langValues.put("Persian", "fa");
        langValues.put("Polish", "pl");
        langValues.put("Portuguese", "pt");
        langValues.put("Romanian", "ro");
        langValues.put("Russian", "ru");
        langValues.put("Serbian", "sr");
        langValues.put("Slovak", "sk");
        langValues.put("Slovenian", "sl");
        langValues.put("Spanish", "es");
        langValues.put("Swahili", "sw");
        langValues.put("Swedish", "sv");
        langValues.put("Thai", "th");
        langValues.put("Turkish", "tr");
        langValues.put("Ukrainian", "uk");
        langValues.put("Vietnamese", "vi");
        langValues.put("Welsh", "cy");
        langValues.put("Yiddish", "yi");
        
    }
    
     public String getLangRepresentation(String val) {
        String repr = langRepresentations.get(val);
        //System.out.println("representation for val: "+val+" -> "+repr);
        return repr;
    }
    
    public String getLangValue(String repr) {
        String val = this.langValues.get(repr);
        //System.out.println("value for repr: "+repr+" -> "+val);
        return val;
    }
    
    public Set<String> listAllLangValues() {
        return this.langRepresentations.keySet();
    }
    
    public Set<String> listAllLangRepresentations() {
        return this.langValues.keySet();
    }
    
    public int size() {
        return langValues.size();
    }
    
}
