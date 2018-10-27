namespace CleanicaFinal
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel.DataAnnotations;
    using System.ComponentModel.DataAnnotations.Schema;
    using System.Data.Entity.Spatial;

    [Table("Cliente")]
    public partial class Cliente
    {
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2214:DoNotCallOverridableMethodsInConstructors")]
        public Cliente()
        {
            Formulario_de_Satisfacao = new HashSet<Formulario_de_Satisfacao>();
            Formulario_de_Realizacao = new HashSet<Formulario_de_Realizacao>();
            ServicosRealizados = new HashSet<ServicosRealizado>();
        }

        [Key]
        [StringLength(50)]
        public string Utilizador_email { get; set; }

        public int telemovel { get; set; }

        [Required]
        [StringLength(100)]
        public string morada { get; set; }

        public virtual Utilizador Utilizador { get; set; }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2227:CollectionPropertiesShouldBeReadOnly")]
        public virtual ICollection<Formulario_de_Satisfacao> Formulario_de_Satisfacao { get; set; }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2227:CollectionPropertiesShouldBeReadOnly")]
        public virtual ICollection<Formulario_de_Realizacao> Formulario_de_Realizacao { get; set; }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2227:CollectionPropertiesShouldBeReadOnly")]
        public virtual ICollection<ServicosRealizado> ServicosRealizados { get; set; }
    }
}
