/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package predicatechecker;

import java.io.File;
import java.util.HashSet;
import java.util.Set;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.MissingImportEvent;
import org.semanticweb.owlapi.model.MissingImportListener;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;

/**
 *
 * @author stb
 */
public class OWLHelper {

    private static class MissingImportHandler implements MissingImportListener {

        public void importMissing(MissingImportEvent evt) {
            System.out.println("import not loaded: "+evt.getImportedOntologyURI().toString());
        }

    }

    public static Set<String> getPredicatesFromOWL(File f) {
        Set<String> predicates = new HashSet<String>();

        try {
            // load ontology
            OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
            // manager.addMissingImportListener(new MissingImportHandler());
            IRI iri = IRI.create(f);
            manager.setSilentMissingImportsHandling(true);
            OWLOntology ont = manager.loadOntology(iri);

            // get classes
            Set<OWLClass> classes = ont.getClassesInSignature();
            for(OWLClass c : classes) {
                predicates.add(c.toStringID());
            }
            // get obj props
            Set<OWLObjectProperty> objectProperties = ont.getObjectPropertiesInSignature();
            for(OWLObjectProperty p : objectProperties) {
                predicates.add(p.toStringID());
            }
            
            // get data props
            Set<OWLDataProperty> dataProperties = ont.getDataPropertiesInSignature();
            for(OWLDataProperty p : dataProperties) {
                predicates.add(p.toStringID());
            }

        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            return predicates;
        }
    }

}
