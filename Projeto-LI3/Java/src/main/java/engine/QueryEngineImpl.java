/**
 * Nesta classe, são respondidas todas as querys necessárias.
 * É implementada uma Interface a qual contém os métodos pedidos.
 * Esta classe tem ainda uma variável de instância, Data, onde estarão guardados
 * todos os dados relativos ao programa.
 *
 * @author André Vieira
 * @author David Sousa
 * @author Carlos Pedrosa
 * @author Manuel Sousa
 */

package engine;

import li3.Interface;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.Collections;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

public class QueryEngineImpl implements Interface {
    /**
     * Variável de Instância.
     */
    private Data data;
    
    /**
     * É inicializado e criado o objeto Data.
     */
    public void init() {
        this.data = new Data();
    }
    
    /** 
     * Método load(int nsnaps, ArrayList<String> snaps_paths)
     * Carrega para as diferentes estruturas todos os dados necessários,
     * através do parse dos snapshots fornecidos.
     *
     * @param nsnaps
     * @param snaps_paths
     */
    public void load(int nsnaps, ArrayList<String> snaps_paths) {
        int i = 0;
        try {
            for (i = 0; i < nsnaps; i++) {
                this.data.parse(this.data, snaps_paths.get(i));
            }
        } 
        catch (XMLStreamException e) {
            System.out.println(e.toString());
        } 
        catch (FileNotFoundException e) {
            System.out.println(e.toString());
        }
    }
    
    /** 
     * Método all_articles()
     * Devolve o númeto total de artigos.
     * 
     * @return long
     */
    public long all_articles() {
        return this.data.getNumAllArticles();
    }
    
    /** 
     * Método unique_articles()
     * Devolve o número de artigos únicos nos vários backups analisados.
     * 
     * @return long
     */
    public long unique_articles() {
        return (long) this.data.getInfoArticle().size();
    }
    
    /** 
     * Método all_revisions()
     * Devolve o número de revisões que foram efetuadas nos backups.
     *
     * @return long
     */
    public long all_revisions() {  
        Map<String, ArticleList> aux = this.data.getInfoArticle();
        
        return (long) aux.values()
                         .stream()
                         .mapToInt(artList -> artList.getArticles().size())
                         .sum();
    }
    
    /** 
     * Método top_10_contributors()
     * Devolve um ArrayList<Long> com os 10 autores que mais contríbuiram para
     * um maior número de revisões.
     *
     * @return ArrayList<Long>
     */
    public ArrayList<Long> top_10_contributors() {
        ArrayList<Long> reps = new ArrayList<Long>();
        ArrayList<Long> top10Cont = new ArrayList<Long>();
        Map<String, Contributor> infoContributor = this.data.getInfoContributor();

        infoContributor.values()
                       .stream()
                       .limit(10)
                       .forEach(art -> {
                            top10Cont.add(Long.parseLong(art.getUserID()));
                            reps.add((long) art.getReps());
                       });

        infoContributor.values()
                       .stream()
                       .forEach(art -> {
                            long aux = (long) art.getReps();
                            int index = reps.indexOf(Collections.min(reps));
                            if (aux > reps.get(index)) {
                                reps.set(index, aux);
                                top10Cont.set(index, Long.parseLong(art.getUserID()));
                            }
                        });
        
        this.data.sortByReps(top10Cont, reps);
        
        return top10Cont;
    }
    
    /**
     * Método contributor_name(long contributor_id)
     * Devolve o nome do autor com determinado identificador.
     *
     * @param contributor_id
     * @return String
     */
    public String contributor_name(long contributor_id) {
        String userID = Long.toString(contributor_id);
        Contributor aux = new Contributor();

        aux = this.data.getInfoContributor().get(userID);
        
        if (aux == null) return "(null)";
        else return aux.getUsername();
    }
    
    /**
     * Método top_20_largest_articles()
     * Devolve um ArrayList<Long> com os identificadores dos 20 maiores artigos.
     *
     * @return ArrayList<Long>
     */
    public ArrayList<Long> top_20_largest_articles() {
        ArrayList<Long> ids = new ArrayList<Long>();
        ArrayList<Long> top_N_Articles = new ArrayList<Long>();
        Map<String, ArticleList> infoArticles = this.data.getInfoArticle();

        infoArticles.values()
                    .stream()
                    .limit(20)
                    .forEach(artList -> {
                        List<Article> list = artList.getArticles();
                        long id = Long.parseLong(artList.getArtID());
                        long size = list.get(list.size() - 1).getSize(); 
                        top_N_Articles.add(size);
                        ids.add(id);
                    });

        infoArticles.values()
                    .stream()
                    .forEach(art -> {
                        long aux = art.getLargestArticle();
                        int index = top_N_Articles.indexOf(Collections.min(top_N_Articles));
                        if (aux > top_N_Articles.get(index)) {
                            top_N_Articles.set(index, aux);
                            ids.set(index, Long.parseLong(art.getArtID()));
                        }
                    });
        
        this.data.sortByReps(ids, top_N_Articles);
        
        return ids;
    }
    
    /**
     * Método article_title(long article_id)
     * Devolve o título de um artigo com determinado identificador.
     *
     * @param article_id
     * @return String
     */
    public String article_title(long article_id) {
        String artID = Long.toString(article_id);
        String title = new String();

        ArticleList artList = this.data.getInfoArticle().get(artID);
       
        if (artList == null) title = "(null)";
        else {
            List<Article> list = artList.getArticles(); 
            title = list.get(list.size() - 1).getTitle();
        }
        return title;
    }
    
    /** 
     * Método top_N_articles_with_more_words(int n)
     * Devolve um ArrayList<Long> com N artigos com maior número de palavras.
     *
     * @param n
     * @return ArrayList<Long>
     */
    public ArrayList<Long> top_N_articles_with_more_words(int n) {
        ArrayList<Long> ids = new ArrayList<Long>();
        ArrayList<Long> top_N_Articles = new ArrayList<Long>();
        Map<String, ArticleList> infoArticles = this.data.getInfoArticle();

        infoArticles.values()
                    .stream()
                    .limit(n)
                    .forEach(artList -> {
                        List<Article> list = artList.getArticles();
                        long id = Long.parseLong(artList.getArtID());
                        long words = list.get(list.size() - 1).getWords();
                        top_N_Articles.add(words);
                        ids.add(id);
                    });

        infoArticles.values()
                    .stream()
                    .forEach(art -> {
                        long aux = art.getMoreWordsArticle();
                        int index = top_N_Articles.indexOf(Collections.min(top_N_Articles));
                        if (aux > top_N_Articles.get(index)) {
                            top_N_Articles.set(index, aux);
                            ids.set(index, Long.parseLong(art.getArtID()));
                        }
                    });

        this.data.sortByReps(ids, top_N_Articles);

        return ids;
    }
    
    /**
     * Método titles_with_prefix(String prefix)
     * Devolve um ArrayList<String> com os títulos de certos artigos com determinado
     * prefixo.
     *
     * @param prefix
     * @return ArrayList<String>
     */
    public ArrayList<String> titles_with_prefix(String prefix) {
        Map<String, ArticleList> aux = this.data.getInfoArticle();

        return aux.values()
                  .stream()
                  .map(artList -> artList.getArticles().get(artList.getArticles().size() - 1).getTitle())
                  .filter(s -> s.startsWith(prefix))
                  .sorted()
                  .collect(Collectors.toCollection(ArrayList::new)); 
    }
    
    /** 
     * Método article_timestamp(long article_id, long revision_id)
     * Devolve o timestamp de uma revisão de um certo artigo.
     *
     * @param article_id
     * @param revision_id
     * @return String
     */
    public String article_timestamp(long article_id, long revision_id) {
        String artID = Long.toString(article_id);
        String revID = Long.toString(revision_id);
        String timestamp = new String();
        
        ArticleList artList = this.data.getInfoArticle().get(artID);

        if (artList == null) timestamp = "(null)";
        else {
            List<Article> list = artList.getArticles();
            boolean match = list.stream()
                                .anyMatch(art -> art.getRevID().equals(revID));
            
            if (match) timestamp = list.stream()
                                       .filter(art -> art.getRevID().equals(revID))
                                       .findFirst()
                                       .get()
                                       .getTimestamp();
            else timestamp = "(null)";
        }
        return timestamp;
    }
    
    /**
     * Método clean()
     * Limpa toda a informação da classe.
     */
    public void clean() {
        /* Método clean está implementado na classe Data, 
           a qual contém as estruturas de dados usadas */
        this.data.clean();
    }
}
