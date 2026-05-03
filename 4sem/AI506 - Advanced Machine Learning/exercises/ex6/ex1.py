sentences = [
    "the film was wonderful and touching",
    "terrible movie very boring and slow",
    "great performances and a beautiful story",
    "awful acting the plot made no sense",
    "loved every moment of this masterpiece",
    "waste of time poorly written script",
]
labels = [1, 0, 1, 0, 1, 0]



def whitespace_tokenize(text: str) -> list[str]:
    lowercase_string = text.lower()
    return set(lowercase_string.split())


def char_tokenize(text: str) -> list[str]:
    lowercase_string = text.lower()
    nospaces_string = lowercase_string.replace(" ", "")
    return set(list(nospaces_string))

# 1. Vocabulary size: how does it scale with corpus size?
    # whitespace_tokenize would take up more space than char_tokenize, as there are more words that characters

# 2. Out-of-vocabulary (OOV) handling: what happens with unseen words?
    # for whitespace_tokenize it would append that word
    # for char_tokenize, the word might just be a new combination, so not so bad

# 3. Sequence length: how does the average sequence length differ between the two?
    # same as 1

class Vocabulary:
    def __init__(self):
        self.token2idx: dict[str, int] = {}
        self.idx2token: dict[int, str] = {}

    def build(self, tokenized_corpus: list[list[str]]) -> None:
        """Populate token2idx and idx2token from a list of tokenized sentences."""
        for tokenized_sentence in tokenized_corpus:
            token2idx = self.encode(tokenized_sentence)
            for token, idx in tokenized_sentence, token2idx:
                self.token2idx(token, idx)
                self.idx2token(idx, token)
            
        pass

    def tokenized_corpus(self, sentences: list[str]) -> list[list[str]]:
        tokenized_corpus_matrix = []
        for sentence in sentences:
            tokenized_corpus_matrix.append(char_tokenize(sentence))
        return tokenized_corpus_matrix

    def encode(self, tokens: list[str]) -> list[int]:
        """Map each token to its index; unknown tokens map to <UNK>."""
        for token in tokens:
            pass

    def decode(self, indices: list[int]) -> list[str]:
        """Map each index back to its token string."""
        # TODO
        pass

    def __len__(self) -> int:
        return len(self.token2idx)