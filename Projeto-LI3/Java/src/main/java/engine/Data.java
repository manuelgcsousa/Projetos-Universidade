/** 
 * Classe Data: Classe responsável pelo manuseamento de todos os dados da
 * aplicação.
 * É aqui que são guardadas as estruturas de dados respetivas a todos os artigos
 * e contribuidores.
 *
 * Para guardar esta informação, tanto para a informação sobre os artigos, bem
 * como a informação sobre os contribuidores, foram usados dois Maps,
 * respetivamente: 
 * - Map<String, ArticleList> infoArticle;
 * - Map<String, Contributor> infoContributor;
 *
 * Em infoArticle a chave String corresponde ao ID de um certo artigo.
 * Em infoContributor a chave String corresponde ao ID de um certo contribuidor.
 *
 * Ainda existe uma variável allArticles, que ao longo do parse de diferentes
 * snapshots, vai incrementando consoante o número de artigos que encontrar.
 *
 * @author André Vieira
 * @author David Sousa
 * @author Carlos Pedrosa
 * @author Manuel Sousa
 */

package engine;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

public class Data {
	/**
     * Variáveis de Instância.
     */
    private Map<String, ArticleList> infoArticle;
	private Map<String, Contributor> infoContributor;
	private long allArticles;
    
    /**
     * Construtor Vazio.
     * Cria uma instância de Data.
     */
	public Data() {
		this.infoArticle = new HashMap<String, ArticleList>();
		this.infoContributor = new HashMap<String, Contributor>();
		this.allArticles = 0;
	}
    
    /**
     * Conjunto de Getters e Setters da classe Data.
     */
	public Map<String, ArticleList> getInfoArticle() {
		Map<String, ArticleList> aux = new HashMap<String, ArticleList>();

		aux = this.infoArticle.values()
							  .stream()
							  .collect(Collectors.toMap((e) -> e.getArtID(),
									   				    (e) -> e.clone()));
		return aux;
	}

	public Map<String, Contributor> getInfoContributor() {
		Map<String, Contributor> aux = new HashMap<String, Contributor>();

		aux = this.infoContributor.keySet()
								  .stream()
								  .collect(Collectors.toMap((e) -> e, 
								  							(e) -> this.infoContributor.get(e).clone()));
		return aux;
	}

	public long getNumAllArticles() {
		return this.allArticles;
	}

	public void setInfoArticle (Map<String, ArticleList> infoArticle) {
		Map<String, ArticleList> aux = new HashMap<String, ArticleList>();

		this.infoArticle.clear();
		this.infoArticle = infoArticle.values()
									  .stream()
									  .collect(Collectors.toMap((e) -> e.getArtID(),
									  							(e) -> e.clone()));
	}

	public void setInfoContributor (Map<String, Contributor> infoContributor) {
		Map<String, Contributor> aux = new HashMap<String, Contributor>();

		this.infoContributor.clear();
		this.infoContributor = infoContributor.keySet()
											  .stream()
											  .collect(Collectors.toMap((e) -> e,
											  							(e) -> infoContributor.get(e).clone()));
	}

	public void setNumAllArticles (long allArticles) {
		this.allArticles = allArticles;
	}
    
    /** 
     * Método auxiliar que verifica se um certo ID existe numa lista de revisões.
     * 
     * @param revList
     * @param revID
     * @return boolean
     */
    public boolean matchID (List<Article> revList, String revID) {
		if (revList == null) return false;
		else return revList.stream()
					       .anyMatch(art -> art.getRevID().equals(revID));
	}
    
    /**
     * Insere a informação respetiva a um artigo no Map "infoArticle".
     * Aqui é criado um objeto Article e ArticleList, e depois inserido no Map.
     * 
     * @param artID
     * @param title
     * @param revID
     * @param timestamp
     * @param size
     * @param words
     */
	public void insertInfoArticle (String artID, String title, String revID, String timestamp, long size, long words) {
		ArticleList artList = this.infoArticle.get(artID);
		Article art = new Article();

		if (artList == null) {
			artList = new ArticleList();
			List<Article> list = new ArrayList<Article>();
			
			art.setArtID(artID);
			art.setTitle(title);
			art.setRevID(revID);
			art.setTimestamp(timestamp);
            art.setSize(size);
            art.setWords(words);

			list.add(art);

			artList.setArtID(artID);
			artList.setArticles(list);

			this.infoArticle.put(artID, artList);
		} else {
			List<Article> revList = artList.getArticles();
			if (!matchID(revList, revID)) {
				art.setArtID(artID);
				art.setTitle(title);
				art.setRevID(revID);
				art.setTimestamp(timestamp);
				art.setSize(size);
                art.setWords(words);

				revList.add(art);

				artList.setArtID(artID);
				artList.setArticles(revList);

				this.infoArticle.replace(artID, artList);
			}
		}
	}
    
    /** 
     * Insere a informação respetiva a um Contribuidor no Map "infoContributor".
     * Se este já existir no Map esse Contributor, é apenas incrementada a sua variável
     * "reps" relativa à sua repetição na edição de certos artigos;
     * Senão, é criado um novo objeto Contributor, com as informações recebidas,
     * e depois inserido no Map.
     * 
     * @param username
     * @param userID
     * @param artID
     * @param revID
     */
	public void insertInfoContributor (String username, String userID, String artID, String revID) {
		if (userID == null) return;
		
		Contributor cont = this.infoContributor.get(userID);
		
		if (cont == null) {
			cont = new Contributor();
			cont.setUserID(userID);
			cont.setUsername(username);
			cont.setReps(1);
			this.infoContributor.put(userID, cont);

		} else {
			ArticleList artList = this.infoArticle.get(artID);
			if (artList == null) artList = new ArticleList();
			List<Article> list = artList.getArticles();
			
			if (!matchID(list, revID)) {
				Contributor aux = cont.clone();
				int reps = aux.getReps();
				reps = reps + 1;
				aux.setReps(reps);

				this.infoContributor.replace(userID, aux);
			}
		}
	}
    
    /** 
     * Método auxiliar que ordena de forma especial um array.
     * Nesta função, são recebidos um ArrayList com certas informações resposta,
     * como o número de palavras de um artigo ou número de repetições, e outro
     * ArrayList com ID's de artigos. O objetivo é ordenar o ArrayList com as
     * informações resposta, e alterar ao mesmo tempo (para as mesmas posições)
     * o ArrayList dos ID's.
     *
     * @param top10Cont
     * @param reps
     */
    public void sortByReps (ArrayList<Long> top10Cont, ArrayList<Long> reps) {
        int i = 0;
        int j = 0;
        int pos;
        long tempReps, tempIDs;
        
        long[] auxReps = new long[reps.size()];
        long[] auxIDs = new long[top10Cont.size()];
        
        for (int k = 0; k < top10Cont.size(); k++) {
            auxReps[k] = reps.get(k);
            auxIDs[k] = top10Cont.get(k);
        }

        for (i = 0; i < top10Cont.size(); i++) {
            pos = i;
            for (j = i + 1; j < top10Cont.size(); j++) {
                if (auxReps[pos] < auxReps[j]) pos = j;
                if (auxReps[pos] == auxReps[j]) {
                    if (auxIDs[pos] > auxIDs[j]) pos = j;
                }
            }
            if (pos != i) {
                tempReps = auxReps[i];
                tempIDs = auxIDs[i];

                auxReps[i] = auxReps[pos];
                auxIDs[i] = auxIDs[pos];

                auxReps[pos] = tempReps;
                auxIDs[pos] = tempIDs;
            }
        }
        
        top10Cont.clear();
        for (int m = 0; m < reps.size(); m++) {
            top10Cont.add(auxIDs[m]);
        }
    }
    
    /** 
     * Devolve uma representação da classe Data em modo textual.
     * 
     * @return String
     */
	public String toString() {
		String info = new String("-> Info Article: \n");
		
		for (String s : this.infoArticle.keySet()) {
			info = info.concat("ID do Article: " + s + "\n" + this.infoArticle.get(s).toString());
		}

		for (String s : this.infoContributor.keySet()) {
			info = info.concat("ID do Contribuidor: " + s + "\n" + this.infoContributor.get(s).toString());
		}

		info = info.concat("All Articles: " + this.allArticles);

		return info; 
	}
    
    /** 
     * Método que limpa toda a informação na classe.
     */
    public void clean() {
        this.infoArticle.values()
                        .stream()
                        .forEach(artList -> artList = null);
        this.infoArticle.clear();
        
        this.infoContributor.values()
                            .stream()
                            .forEach(cont -> cont = null);
        this.infoContributor.clear();
        
        this.allArticles = 0;
    }

    /** 
     * Método auxiliar que conta as palavras e bytes de um certo artigo.
     * É devolvido um array cuja primeira posição contém o número de palavras e
     * a segunda contém o número de bytes.
     * 
     * @param article
     * @return int[]
     */
    public int[] contaPalavras (String article) {
        int i;
        int palavras = 0;

        //i é igual aos caracteres
        for (i = 0; i < article.length() - 1; i++) {
            if ((article.charAt(i) != ' ') && (article.charAt(i) != '\n') && (article.charAt(i) != '\t') && 
                ((article.charAt(i + 1) == ' ') || (article.charAt(i + 1) == '\t') || (article.charAt(i + 1) == '\n'))) {
                palavras++;
            }
        } 
        
        int words_size[] = new int[2];
        words_size[0] = palavras;
        words_size[1] = i - 1;

        return words_size;
    }
    
    /**
     * Aqui é feito o parse dos snapshots, e é enviada a informação para os
     * métodos "insertInfoArticle" e "insertInfoContributor", para assim
     * construir os Maps com todos os dados.
     * São ainda enviadas duas excepções em caso de erro: 
     * XMLStremException e FileNotFoundException.
     *
     * @param data
     * @param snapshot
     */
    public void parse (Data data, String snapshot) throws XMLStreamException, FileNotFoundException {
        String artID = new String();
        String title = new String(); 
        String revID = new String(); 
        String timestamp = new String(); 
        String username = new String();
        String userID = new String();

        List<Article> list = null;
        ArticleList artList = null;
        Article art = null;
        Contributor cont = null;
        
        int i = 0;
        int flag = 0;
        int size = 0;
        int words = 0;
        long allArticles = data.getNumAllArticles();

        String tagContent = null;
        XMLInputFactory factory = XMLInputFactory.newInstance();
        XMLStreamReader reader = factory.createXMLStreamReader(new FileInputStream(snapshot));
        
        while (reader.hasNext()) {
            int event = reader.next();
            switch(event) {
                case XMLStreamConstants.START_ELEMENT: 
                    if ("page".equals(reader.getLocalName())) {
                        list = new ArrayList<Article>();
                        art = new Article();
                        cont = new Contributor();
                    }
                    
                    if ("mediawiki".equals(reader.getLocalName())) {
                        artList = new ArticleList();
                    }
                    
                    if ("text".equals(reader.getLocalName())) {
                        int words_size[] = new int[2];
                        String article = reader.getElementText();
                        words_size = contaPalavras(article);
                        words = words_size[0];
                        size = words_size[1];
                    }
                    
                    break;
                
                case XMLStreamConstants.CHARACTERS:
                    tagContent = reader.getText();///.trim();
                    break;
                
                case XMLStreamConstants.END_ELEMENT:
                    String tag = reader.getLocalName();
                    
                    if (tag.equals("page")) {
                        allArticles++;
                        data.insertInfoContributor(username, userID, artID, revID);
                        data.insertInfoArticle(artID, title, revID, timestamp, (long) size, (long) words);
                        data.setNumAllArticles(allArticles);
                        size = 0;
                        words = 0;
                    } 
                    
                    else if (tag.equals("title")) {
                        title = tagContent;
                    } 
                    
                    else if (tag.equals("ip")) {
                        userID = null;
                        flag = 0;
                    } 
                    
                    else if (tag.equals("id")) {
                        switch (flag) {
                            case 0: 
                                artID = tagContent;
                                flag = 1;
                                break;
                            case 1:
                                revID = tagContent;
                                flag = 2;
                                break;
                            case 2:
                                userID = tagContent;
                                flag = 0;
                                break;
                        }
                    } 
                    
                    else if (tag.equals("timestamp")) {
                        timestamp = tagContent;
                    } 
                    
                    else if (tag.equals("username")) {
                        username = tagContent;
                    }

                    break;
            }
        }
    } 
}
