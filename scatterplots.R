library(ggplot2)

cats = c("Onset", "Nucleus", "Coda", "Tone")
monosyl_weights = c(1.80, 2.12, .68, .84)
monosyl_entropies = c(3.891197, 2.868783, 2.669176, 2.529513)
monosyl_fl =  c(0.16188311, 0.08906263, 0.04872228, 0.10079986)
monosyl = data.frame(cats, monosyl_weights, monosyl_entropies, monosyl_fl)
ent_mono_scatter = ggplot(monosyl, aes(x = monosyl_entropies, y = monosyl_weights)) + geom_text(aes(label=cats), size=4.5, nudge_y = .1) + geom_point() + ylab("Weights in the distance model") + xlab("Entropies") + ggtitle("Entropy and distance weightings (Monosyllables)")
ggsave("ent_mono_scatter.svg", ent_mono_scatter)
fl_mono_scatter = ggplot(monosyl, aes(x = monosyl_fl, y = monosyl_weights)) + geom_text(aes(label=cats), size=4.5, nudge_y = .1) + geom_point() + ylab("Weights in the distance model") + xlab("FLs") + ggtitle("FL and distance weightings (Monosyllables)")
ggsave("fl_mono_scatter.svg", fl_mono_scatter)

disyl_weights = c(2.53, 1.38, 0.98, 1.29)
disyl_entropies = c(6.833218, 5.415708, 5.308119, 4.736466)
disyl_fl = c(0.013085529, 0.002564333, 0.001022829, 0.004494932)
disyl = data.frame(cats, disyl_weights, disyl_entropies, disyl_fl)
ent_di_scatter = ggplot(disyl, aes(x = disyl_entropies, y = disyl_weights)) + geom_text(aes(label=cats), size=4.5, nudge_y = .1) + geom_point() + ylab("Weights in the distance model") + xlab("Entropies") + ggtitle("Entropy and distance weightings (Disyllables)")
ggsave("ent_di_scatter.svg", ent_di_scatter)
fl_di_scatter = ggplot(disyl, aes(x = disyl_fl, y = disyl_weights)) + geom_text(aes(label=cats), size=4.5, nudge_y = .1) + geom_point() + ylab("Weights in the distance model") + xlab("FLs in the distance model") + ggtitle("FLs and distance weightings (Disyllables)")
ggsave("fl_di_scatter.svg", fl_di_scatter)
