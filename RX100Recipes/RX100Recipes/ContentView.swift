import SwiftUI
import SwiftData

struct ContentView: View {
    @Environment(RecipeStore.self) private var store
    @Query private var storedUserRecipes: [UserRecipe]

    var body: some View {
        TabView {
            RecipeListView()
                .tabItem { Label("Recipes", systemImage: "camera.filters") }

            FavoritesView()
                .tabItem { Label("Favorites", systemImage: "star.fill") }

            PPSlotsView()
                .tabItem { Label("Camera Memory", systemImage: "square.grid.2x2") }

            NotesListView()
                .tabItem { Label("My Notes", systemImage: "note.text") }
        }
        .onAppear {
            store.userRecipes = storedUserRecipes.map { $0.asRecipe }
        }
        .onChange(of: storedUserRecipes) { _, new in
            store.userRecipes = new.map { $0.asRecipe }
        }
    }
}
