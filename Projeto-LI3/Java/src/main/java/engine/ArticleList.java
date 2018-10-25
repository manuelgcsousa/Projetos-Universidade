/**
 * A classe ArticleList é a classe que suporta as diferentes revisões de um
 * artigo.
 * Aqui, é guardada uma List<Article> com as diferentes revisões de um
 * determinado artigo, identificado pela variável artID.
 *
 * Ao guardar a informação sobre cada revisão, temos a possibilidade de aceder a
 * cada revisão individualmente, bem como seus dados.
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

public class ArticleList {
	/**
     * Variáveis de Instância.
     */
    private String artID;
	private List<Article> articles;
    
    /**
     * Construtor Vazio.
     */
	public ArticleList() {
		this.artID = "";
		this.articles = new ArrayList<Article>();
	}
    
    /**
     * Construtor por parâmetro.
     * 
     * @param artID
     * @param articles
     */
	public ArticleList (String artID, List<Article> articles) {
		List<Article> aux = new ArrayList<Article>();
		this.artID = artID;

		aux = articles.stream()
					  .map(Article::clone)
					  .collect(Collectors.toList());

		this.articles = aux;
	}
    
    /**
     * Construtor por cópia.
     * 
     * @param artList
     */
	public ArticleList (ArticleList artList) {
		this.artID = artList.getArtID();
		this.articles = artList.getArticles();
	}
    
    /**
     * Conjunto de Getters e Setters da classe ArticleList.
     */
	public String getArtID() {
		return this.artID;
	}

	public List<Article> getArticles() {
		return this.articles.stream()
				    		.map(Article::clone)
					        .collect(Collectors.toList());
	}

	public void setArtID (String artID) {
		this.artID = artID;
	}

	public void setArticles (List<Article> articles) {
		this.articles.clear();
		this.articles = articles.stream()
						   	    .map(Article::clone)
						        .collect(Collectors.toList());
	}
    
    /**
     * Devolve a revisão do artigo com maior tamanho (bytes).
     * 
     * @return long
     */
	public long getLargestArticle() {
		List<Long> list = this.articles.stream()
					 		  		   .map(art -> art.getSize())
					 		  		   .sorted()
					 		  		   .collect(Collectors.toList());

		return list.get(list.size() - 1);
	}
    
    /**
     * Devolve a revisão do artigo com mais palavras.
     * 
     * @return long
     */
	public long getMoreWordsArticle() {
		List<Long> list = this.articles.stream()
									   .map(art -> art.getWords())
									   .sorted()
									   .collect(Collectors.toList());

		return list.get(list.size() - 1);
	} 
	
    /**
     * Devolve uma representação da classe ArticleList em modo textual.
     * 
     * @return String
     */
	public String toString() {
       	String aux = new String ("-> ArticleList: \n");
        
        for (int i = 0; i < this.articles.size(); i++) {
            Article a = this.articles.get(i);
            aux = aux.concat(a.toString() + "\n");
        }         
        
        return aux;
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

        ArticleList aux = (ArticleList) o;

        return (this.artID.equals(aux.getArtID()) && this.articles.equals(aux.getArticles()));
    } 

    /** 
     * Retorna uma cópia da instância.
     * 
     * @return ArticleList
     */
	public ArticleList clone() {
		return new ArticleList(this);
	}
}
