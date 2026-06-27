import SwiftUI
import SwiftData

struct FavoritesView: View {
    @Environment(RecipeStore.self) private var store
    @Query private var favorites: [RecipeFavorite]
    @Query private var allNotes: [RecipeNote]

    private var favoriteRecipes: [Recipe] {
        let ids = Set(favorites.map { $0.recipeId })
        return store.allRecipes
            .filter { ids.contains($0.id) }
            .sorted { $0.name < $1.name }
    }

    var body: some View {
        NavigationStack {
            Group {
                if favoriteRecipes.isEmpty {
                    ContentUnavailableView(
                        "No Favorites Yet",
                        systemImage: "star",
                        description: Text("Tap the star on any recipe to save it here.")
                    )
                } else {
                    List(favoriteRecipes) { recipe in
                        NavigationLink {
                            RecipeDetailView(recipe: recipe)
                        } label: {
                            RecipeRowView(recipe: recipe, isFavorite: true)
                        }
                    }
                    .listStyle(.insetGrouped)
                }
            }
            .navigationTitle("Favorites")
        }
    }
}
