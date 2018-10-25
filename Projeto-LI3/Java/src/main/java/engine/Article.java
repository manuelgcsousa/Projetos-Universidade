/**
 * Classe que guarda a informação sobre um determinado artigo.
 * Nesta classe são guardadas as informações necesserárias sobre um determinado
 * artigo consoante as querys que pretendemos responder.
 * 
 * Aqui guardamos o ID do artigo;
 * O título de um artigo; 
 * O ID da revisão de um artigo; 
 * O timestamp de um artigo; 
 * O tamanho e o número de palavras de um artigo.
 *
 * @author André Vieira
 * @author David Sousa
 * @author Carlos Pedrosa
 * @author Manuel Sousa
 */

package engine;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class Article {
    /** 
     * Variáveis de Instãncia.
     */
	private String artID;
	private String title;
	private String revID;
	private String timestamp;
	private long size;
	private long words; 
    
    /**
     * Construtor Vazio.
     */
	public Article() {
		this.artID = "";
		this.title = "";
		this.revID = "";
		this.timestamp = "";
		this.size = 0;
		this.words = 0;
	}
    
    /**
     * Construtor por parâmetro.
     * 
     * @param artID
     * @param title
     * @param revID
     * @param timestamp
     * @param size
     * @param words
     */
	public Article (String artID, String title, String revID, String timestamp, long size, long words) {
		this.artID = artID;
		this.title = title;
		this.revID = revID;
		this.timestamp = timestamp;
		this.size = size;
		this.words = words;
	}
    
    /**
     * Construtor por cópia.
     * 
     * @param art
     */
	public Article (Article art) {
		this.artID = art.getArtID();
		this.title = art.getTitle();
		this.revID = art.getRevID();
		this.timestamp = art.getTimestamp();
		this.size = art.getSize();
		this.words = art.getWords();
	}
    
    /** 
     * Conjunto de Getters e Setters da classe Article.
     */
	public String getArtID() {
		return this.artID;
	}

	public String getTitle() {
		return this.title;
	}

	public String getRevID() {
		return this.revID;
	}

	public String getTimestamp() {
		return this.timestamp;
	}

	public long getSize() {
		return this.size;
	}
    
    public long getWords() {
        return this.words;
    }

	public void setArtID (String artID) {
		this.artID = artID;
	}

	public void setTitle (String title) {
		this.title = title;
	}

	public void setRevID (String revID) {
		this.revID = revID;
	}

	public void setTimestamp (String timestamp) {
		this.timestamp = timestamp;
	}

	public void setSize (long size) {
		this.size = size;
	}
    
    public void setWords (long words) {
        this.words = words;
    }
    
    /**
     * Devolve uma representação da classe Article em modo textual.
     * 
     * @return String 
     */
	public String toString() {
		return ("Title: " + this.title + "\nID do Artigo: " + this.artID + 
				"\nID da Revisão: " + this.revID + "\nTimestamp: " + this.timestamp);
	}
    
    /**
     * Compara igualdade com outro objeto.
     * 
     * @param o
     * @return boolean
     */
	public boolean equals (Object o) {
        if (this == o) return true;
        
        if ((o == null) || (this.getClass() != o.getClass())) return false;
        
        Article aux = (Article) o;
        
        return (this.title.equals(aux.getTitle()) && this.artID == aux.getArtID() && 
        		this.revID.equals(aux.getRevID()) && this.timestamp.equals(aux.getTimestamp()) && 
                this.size == aux.getSize() && this.words == aux.getWords());
    }
    
    /**
     * Retorna uma cópia da instância.
     * 
     * @return Article
     */
	public Article clone() {
		return new Article(this);
	}
}
