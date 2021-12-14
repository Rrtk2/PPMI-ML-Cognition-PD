# Results:

#OwnClass_All_cforest 95% CI: 0.816-0.981 (DeLong)
#OwnClass_All_svmLinear 95% CI: 0.746-0.953 (DeLong)
#OwnClass_All_glmnet 95% CI: 0.857-0.992 (DeLong)
#OwnClass_All_rf 95% CI: 0.791-0.977 (DeLong)
#OwnClass_Clinical_cforest 95% CI: 0.815-0.988 (DeLong)
#OwnClass_Clinical_svmLinear 95% CI: 0.789-0.971 (DeLong)
#OwnClass_Clinical_glmnet 95% CI: 0.795-0.974 (DeLong)
#OwnClass_Clinical_rf 95% CI: 0.831-1 (DeLong)
#OwnClass_Serum_cforest 95% CI: 0.49-0.802 (DeLong)
#OwnClass_Serum_svmLinear 95% CI: 0.545-0.84 (DeLong)
#OwnClass_Serum_glmnet 95% CI: 0.548-0.843 (DeLong)
#OwnClass_Serum_rf 95% CI: 0.418-0.739 (DeLong)
#OwnClass_Genetic_Epigenetic_cforest 95% CI: 0.408-0.73 (DeLong)
#OwnClass_Genetic_Epigenetic_svmLinear 95% CI: 0.338-0.665 (DeLong)
#OwnClass_Genetic_Epigenetic_glmnet 95% CI: 0.284-0.608 (DeLong)
#OwnClass_Genetic_Epigenetic_rf 95% CI: 0.353-0.68 (DeLong)
#LancetClass_All_cforest 95% CI: 0.776-0.963 (DeLong)
#LancetClass_All_svmLinear 95% CI: 0.805-0.966 (DeLong)
#LancetClass_All_glmnet 95% CI: 0.687-0.927 (DeLong)
#LancetClass_All_rf 95% CI: 0.768-0.941 (DeLong)
#LancetClass_Clinical_cforest 95% CI: 0.751-0.937 (DeLong)
#LancetClass_Clinical_svmLinear 95% CI: 0.79-0.947 (DeLong)
#LancetClass_Clinical_glmnet 95% CI: 0.743-0.95 (DeLong)
#LancetClass_Clinical_rf 95% CI: 0.695-0.911 (DeLong)
#LancetClass_Serum_cforest 95% CI: 0.616-0.869 (DeLong)
#LancetClass_Serum_svmLinear 95% CI: 0.656-0.914 (DeLong)
#LancetClass_Serum_glmnet 95% CI: 0.671-0.911 (DeLong)
#LancetClass_Serum_rf 95% CI: 0.534-0.817 (DeLong)
#LancetClass_Genetic_Epigenetic_cforest 95% CI: 0.367-0.643 (DeLong)
#LancetClass_Genetic_Epigenetic_svmLinear 95% CI: 0.321-0.552 (DeLong)
#LancetClass_Genetic_Epigenetic_glmnet 95% CI: 0.446-0.732 (DeLong)
#LancetClass_Genetic_Epigenetic_rf 95% CI: 0.352-0.636 (DeLong)

# Class
for( i_class in c(0,1)){

	# Subset
	for( o_subset in c("All","Clinical","Serum","Genetic_Epigenetic")){ #c("All","Clinical","Serum","Genetic_Epigenetic")

		# Machine learning algorithm
		for( p_algo in c("cforest","svmLinear","glmnet", "rf")){ # svmLinear/glmnet problems ; c("cforest","svmLinear","glmnet", "rf")
		
			temp_Lancet = ifelse(i_class,"LancetClass","OwnClass")
			
			load(paste0("C:/DATA STORAGE/Projects/PPMI/Results/",temp_Lancet,"/",o_subset,"/",p_algo,"_image.RData"))
				
			a=pROC::ci.auc(superresult$Combine_data_main$model[[1]]$AUCval_test)
			
			print(paste0(temp_Lancet,"_",o_subset,"_",p_algo," 95% CI: ",round(a[1],3),"-",round(a[3],3)," (DeLong)"))
			
			 

		}

	}


}

