#!/usr/bin/env python

from pathlib import Path
from dataclasses import dataclass
from typing import List, Optional
import json
import click
from reportlab.pdfgen import canvas
from reportlab.pdfbase import pdfform
from pdfrw import PdfReader, PdfWriter
from pdfrw.buildxobj import pagexobj
from pdfrw.objects.pdfdict import PdfDict


@dataclass
class Question:
    question: str
    response: Optional[str] = None


@dataclass
class QuestionGroup:
    groupid: str
    questions: List[Question]


def load_questions(input_path: Path) -> List[QuestionGroup]:
    data = json.loads(input_path.read_text())
    groups = []
    for group in data:
        questions = [Question(**q) for q in group["questions"]]
        groups.append(
            QuestionGroup(groupid=group["groupid"], questions=questions))
    return groups


def generate_pdf(groups: List[QuestionGroup], output_path: Path) -> None:
    form_height = 90
    offset_te = form_height + 20
    x_offset = 40

    height = 2 * offset_te
    for group in groups:
        for question in group.questions:
            height += offset_te


    c = canvas.Canvas(str(output_path), pagesize=(900, height))
    y_position = height - offset_te
    form = c.acroForm

    for group in groups:
        c.drawString(x_offset, y_position, f"Group: {group.groupid}")
        y_position -= form_height

        for idx, question in enumerate(group.questions):
            field_name = f"{group.groupid}_{idx}_{question.question}"
            c.drawString(x_offset, y_position, question.question)
            form.textfield(
                name=field_name,
                x=x_offset,
                y=y_position - form_height,
                width=500,
                height=form_height,
                maxlen=0,  # No length limit
                fieldFlags="richText multiline",  # Enable rich text for word wrapping
            )
            y_position -= offset_te

        y_position -= form_height

    c.save()


def read_pdf_responses(pdf_path: Path) -> List[QuestionGroup]:
    reader = PdfReader(str(pdf_path))
    form_data = reader.Root.AcroForm.Fields

    responses = {}
    for field in form_data:
        if field.V:
            field_name = field.T[1:-1] if field.T.startswith("(") else field.T
            group_id, q_idx, *question_parts = field_name.split("_")
            q_idx = q_idx.rstrip(")")
            question_text = "_".join(question_parts)

            if group_id not in responses:
                responses[group_id] = {}
            responses[group_id][int(q_idx)] = {
                "question": question_text,
                "response":
                field.V[1:-1] if field.V.startswith("(") else field.V
            }

    groups = []
    for group_id, answers in responses.items():
        questions = []
        for idx, data in answers.items():
            questions.append(
                Question(question=data["question"], response=data["response"]))
        groups.append(QuestionGroup(groupid=group_id, questions=questions))

    return groups


@click.group()
def cli():
    pass


@cli.command()
@click.argument("input_json", type=click.Path(exists=True, path_type=Path))
@click.argument("output_pdf", type=click.Path(path_type=Path))
def export(input_json: Path, output_pdf: Path):
    groups = load_questions(input_json)
    generate_pdf(groups, output_pdf)


@cli.command()
@click.argument("input_pdf", type=click.Path(exists=True, path_type=Path))
@click.argument("output_json", type=click.Path(path_type=Path))
def import_data(input_pdf: Path, output_json: Path):
    groups = read_pdf_responses(input_pdf)
    output_json.write_text(
        json.dumps([{
            "groupid":
            g.groupid,
            "questions": [{
                "question": q.question,
                "response": q.response
            } for q in g.questions]
        } for g in groups],
                   indent=2))
    print("ok")


if __name__ == "__main__":
    cli()
