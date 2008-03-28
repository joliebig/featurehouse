package cide.features;

import java.util.Set;

import cide.gast.ASTNode;

public interface IASTColorProvider {

	public abstract Set<IFeature> getOwnColorsI(ASTNode node);

	public abstract Set<IFeature> getColorsI(ASTNode node);

	public abstract Set<IFeature> getInheritedColorsI(ASTNode node);

}