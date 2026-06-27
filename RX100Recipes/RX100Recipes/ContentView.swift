import SwiftUI

struct ContentView: View {
    var body: some View {
        TabView {
            RecipeListView()
                .tabItem {
                    Label("Recipes", systemImage: "camera.filters")
                }

            FavoritesView()
                .tabItem {
                    Label("Favorites", systemImage: "star.fill")
                }

            NotesListView()
                .tabItem {
                    Label("My Notes", systemImage: "note.text")
                }
        }
    }
}
