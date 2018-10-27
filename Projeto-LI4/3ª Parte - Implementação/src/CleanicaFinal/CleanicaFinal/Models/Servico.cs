namespace CleanicaFinal
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel.DataAnnotations;
    using System.ComponentModel.DataAnnotations.Schema;
    using System.Data.Entity.Spatial;

    [Table("Servico")]
    public partial class Servico
    {
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2214:DoNotCallOverridableMethodsInConstructors")]
        public Servico()
        {
            Funcionarios = new HashSet<Funcionario>();
        }

        [Key]
        public int id_servico { get; set; }

        [Required]
        [StringLength(15)]
        public string nome { get; set; }

        [Required]
        [StringLength(100)]
        public string descricao { get; set; }

        public float preco { get; set; }

        public float precoSProduto { get; set; }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2227:CollectionPropertiesShouldBeReadOnly")]
        public virtual ICollection<Funcionario> Funcionarios { get; set; }
    }
}
